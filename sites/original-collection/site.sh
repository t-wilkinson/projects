#!/usr/bin/bash
# Script to streamline management of my old websites

set -e

if [[ $# < 1 ]]; then
    echo "Requires at least one argument"
    exit 1
fi

from="$HOME/dev/t-wilkinson/projects/sites"
to="$HOME/dev/t-wilkinson/projects/sites/main-server/production"
server="klean-studios"

################################################################################
# ERROR
################################################################################
site-error() {
    echo "Error: Arguments allowed $1, recieved $2"
    exit 1
}

################################################################################
# START
################################################################################
site-start() {
    JS_RUNNER="$HOME/dev/t-wilkinson/sites/js-runner"
    case $1 in
        portfolio)
            silent-ssh $server <<EOF
cd /www/portfolio;
yarn run start
EOF
            ;;

        -f|frontend) npm start;;
        -b|backend) ghcid -c stack repl --restart package.yaml --test Main.main --allow-eval;;
        -n|nginx) nginx -c `pwd`/nginx.conf;;
        -c|css)
            if [[ $2 ]]; then
                [[ -d $2 ]] || (echo "Error: Must be a valid dir"; exit 1)
            fi
            dir=`realpath ${2:-.}`
            mkdir -p $JS_RUNNER/tmp
            ln -f $dir/{public/assets/css/index.css,tailwind.css} $JS_RUNNER/tmp
            ln -f $dir/tailwind.config.js $JS_RUNNER
            cd JS_RUNNER
            npx gulp
            ;;
        *) error "{-c|css|-f|frontend|-b|backend|-n|nginx}" $1;;
    esac
}

################################################################################
# STOP
################################################################################
site-stop() {
    # manually assign pid to nix-shell to delete it later?
    case $1 in
        -n|nginx) nginx -s quit;;
        -[0-9]+) fuser -k $(cut -b 2- - <<< $1)/tcp;;
        *) error "{-f|frontend|-b|backend|-n|nginx|-c|-css}" $1
    esac
}

################################################################################
# RESTART
################################################################################
site-restart() {
    case $1 in
        -n|nginx) nginx -s reload -c `pwd`/nginx.conf;;
        *) error "-n|nginx" $1;;
    esac
}

################################################################################
# REPL
################################################################################
site-repl() {
    case $1 in
        -f|frontend) node;;
        -b|backend) stack repl;;
        *) error "{-f|frontend|-b|backend}" $1;;
    esac
}

################################################################################
# CLEAN
################################################################################
site-clean() {
    case $1 in
        -c|css)
            dir="$HOME/dev/t-wilkinson/projects/sites/js-runner"
            rm -rf $dir/tmp
            rm -f $dir/tailwind.config.js
            rm -f $dir/styles.css
            ;;
        -b|backend)
            cd $from/backend
            stack clean
            ;;
        *) error "{-c|css}" $1;;
    esac
}

################################################################################
# BUILD
################################################################################
silent-ssh() {
    cmd=${2:-$(cat)}
    ssh -T $1 $cmd
}

build-backend() {
    cd $from/backend

    # Build server using stack and write executable
    serverbin=$(stack build backend:server |& grep 'Installing executable server' | cut -d ' ' -f 5)
    if [[ -z $serverbin ]]; then
        echo "Server was previously built. To rebuild run 'site clean backend' and run this command again."
        return 1
    fi

    # Copy executable to remote server
    sftp $server <<EOF
put $serverbin/server /www/server-bin.tmp
EOF
    # Kill current server process, replace with new executable
    silent-ssh $server <<EOF
cd /www
pkill server-bin
mv -f server-bin.tmp server-bin
nohup ./server-bin &> /dev/null &
EOF

}

frontend() {
    # Make sure to...
    # set 'purge: {enabled: true}' in tailwind.config.js
    # Run 'site start -c'.
    if [[ -z $1 ]]; then
        return
    fi
    cd $from/$1
    npm run build
    rsync -avz -r --update --delete $from/$1/build/ $server:/www/$1/
}

renew () {
    silent-ssh $server <<EOF
certbot renew
EOF
}

misc () {
    cd $to
    sftp $server<<EOF
put data.json /www/data.json
put nginx.conf /www/nginx.conf
put nginx.conf /etc/nginx/nginx.conf
EOF
silent-ssh $server <<EOF
nginx -s reload
EOF
}

build-penguins () {
    cd "$from/penguin-sweaters"
    ( cd next
        silent-ssh $server <<EOF
mkdir -p /www/penguins/next
EOF
        rsync -avz -r --update --delete --exclude node_modules $server:/www/penguins/next
        rsync -avz -r --update --delete --exclude '.next/cache' .next $server:/www/penguins/next
        rsync -avz -r --update --delete package.json $server:/www/penguins/next
    )

    ( cd strapi
        ssh $server mkdir -p /www/penguins/strapi
        npx strapi build
        rsync -avz -r --update --delete . $server:/www/penguins/strapi
        # ssh $server npm run start
    )
}

build-neural-style () {
    cd "$from/neural-style"
    silent-ssh $server <<EOF
mkdir -p /www/neural-style
EOF
    rsync -avz -r --update --delete --exclude node_modules --exclude .next/cache $server:/www/neural-style
}

build-portfolio() {
    cd "$HOME/dev/t-wilkinson/portfolio/portfolio-svelte"
    yarn run build
    git add -A && git commit -m '[automated]' && git push || true
    silent-ssh $server "mkdir -p /www/portfolio"
    rsync -avz -r --update --delete --exclude node_modules --exclude .git --exclude .next/cache ./ $server:/www/portfolio

    silent-ssh $server <<"EOF"
cd /www/portfolio;
yarn install;
kill $(lsof -t -i:3002) || true;
yarn run start --port 3002;
EOF

    return

#     silent-ssh $server <<EOF
#     set -e
#     cd /www/portfolio
#     git pull
# EOF
#
#     ssh -tt $server <<EOF
# cd /www/portfolio
# yarn run build
# exit 0
# EOF

}

build-ysa-website() {
cd "$HOME/dev/t-wilkinson/ysa-website"
(
    cd ./client
    NODE_ENV=production yarn run build
    rsync -avz -r --update --delete ./build $server:/www/ysa-website/client
) &
(
    cd ./server
    NODE_ENV=production yarn run build
    rsync -avz -r --update --delete ./build $server:/www/ysa-website/server
) &

wait

silent-ssh $server <<EOF
cd /www/ysa-website ;
(
    cd client
    fuser -k 3000/tcp
    nohup node build/index.js &
)&
(
    cd server
    git pull
    fuser -k 1337/tcp
    nohup yarn run start &
)&
EOF
}

build-klean-studios() {
    project_dir=$HOME/dev/t-wilkinson/klean-studios
    case $1 in
        sync)
            rsync --delete -azhv "${project_dir}/scripts/" klean-studios:/www/klean-studios/scripts/
            rsync --delete -azhv "${project_dir}/client/" klean-studios:/www/klean-studios/client/ --exclude node_modules --exclude build --exclude .env --exclude config/database.js
            rsync --delete -azhv "${project_dir}/server/" klean-studios:/www/klean-studios/server/ --exclude node_modules --exclude public/uploads --exclude build --exclude .env
            ;;
        client)
            silent-ssh $server <<EOF
cd /www/klean-studios/client;
yarn run build;
fuser -k 3001/tcp;
PORT=3001 nohup node build &
EOF
            ;;

        server)
            silent-ssh $server <<EOF
cd /www/klean-studios/server
NODE_ENV=production yarn run build --no-optimization
fuser -k 1338/tcp
NODE_ENV=production nohup yarn run start &
EOF
            ;;

    esac
}

site-build() {
    echo from="$from"
    echo to="$to"
    echo server="$server"
    case $1 in
        klean-studios) build-klean-studios ${@:2};;
        p|portfolio) build-portfolio;;
        dogwalking) frontend dogwalking;;
        penguins) build-penguins;;
        neural-style) build-neural-style;;
        renew) renew;;
        ysa) build-ysa-website;;
        b|backend) build-backend ;;
        m|misc) misc ;;
        *) site-error "{frontend|backend}" $1
    esac
}

################################################################################
# SETUP
################################################################################
site-setup() {
slient-ssh $server <<EOF
sudo apt install certbot
sudo apt install rsync
EOF
}

site-help() {
    echo <<EOF
EOF
}

################################################################################
# MAIN
################################################################################
cmd=$1
shift
case $cmd in
    s|start) site-start $@;;
    stop) site-stop $@;;
    r|restart) site-restart $@;;
    repl) site-repl $@;;
    b|build) site-build $@;;
    c|clean) site-clean $@;;
    h|help) nvim "$(dirname "$0")/site.sh";;
    *) echo "Bad command: must be {restart|start|repl|build|clean}"; exit 1;;
esac

