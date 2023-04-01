# /proc/net/tcp has sockets of interest. we want to listen to these, identify http requests, and return them

import sys
import socket

def hex_host_to_dec(host):
    # python -c 'import sys; a=sys.argv[1]; bytes=reversed([int(a[i:i+2],16) for i in range(0, len(a), 2)]); res=".".join(str(byte) for byte in bytes); print(res)' 0100007F
    host_bytes = reversed([int(host[i:i+2], 16) for i in range(0, len(host), 2)])
    return ".".join(str(byte) for byte in host_bytes)

def hex_port_to_dec(port):
    return int(port, 16)

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    host, port = sys.argv[1].split(':')

    s.bind((hex_host_to_dec(host), hex_port_to_dec(port)))
    s.listen()

    conn, addr = s.accept
    with conn:
        print(f"connected by {addr}")
        while True:
            data = conn.recv(1024)
            if not data:
                break
            conn.sendall(data)
