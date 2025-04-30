set -e

for p in $buildInputs; do
    export PATH=$p/bin${PATH:+:}$PATH
done

mkdir -p "$out/bin"
export PATH="$out/bin":$PATH
echo "$nginxCmd" > $out/bin/nginx
chmod +x $out/bin/nginx
