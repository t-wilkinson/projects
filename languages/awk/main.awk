BEGIN { print "BEGIN" }

/Test/ { print $4 }
{ print $1 ~ /Test/ > "./test.me" }

{  fig($0) }
{ nr() }

function nr() {
    print NR $0
    # print ARGC
    # print ARGV[0]
}

function fig(arg) {
    system("/usr/bin/figlet -f lean " arg " | /usr/bin/tr  ' _/' '/  '")
}

# Test this boi

END { print "END" }
