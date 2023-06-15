#!/usr/bin/bash

while [ $# -ge 1 ]; do
    case $1 in
        clear)
            iptables -P INPUT ACCEPT
            iptables -P FORWARD ACCEPT
            iptables -P OUTPUT ACCEPT

            iptables -Z
            for table in filter nat mangle raw; do
                iptables -t $table -F
                iptables -t $table -X
            done
            ;;

        netedit)
            # sysctl net.ipv4.conf.<interface>.forwarding=1

            # Setup DNAT for netedit
            # iptables -t nat -I PREROUTING --src 0/0 --dst 127.0.0.1 -p tcp --dport 80 -j REDIRECT --to-ports 11110
            iptables -t nat -I OUTPUT -p tcp --dport 80 -j REDIRECT --to-ports 11110
            iptables -t nat -L -n -v
            ;;

        backup)
            iptables-save > iptables.conf
            ip6tables-save > ip6tables.conf
            ;;

        list)
            iptables-save
            ;;

        help)
            ;&
        *)
            echo "iptables.sh clear|netedit|backup|list"
            ;;
    esac

    shift
done
