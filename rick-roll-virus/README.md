## Goals
- Intercept linking process to insert code that writes the actual virus. Or... add a binary onto path that hides gcc, calls gcc with arguments, gets the output binary, and rewrite it there.
- In terms of places to insert virus code, we can insert it into gcc, which would require sudo privaleges, or we can listen to calls.
- Virus should be difficult to disable. IE removing a file, stopping a server, etc. shouldn't stop the virus from working.
- The virus should intercept every http get request and return a rick-roll link. ways we can achieve this:
    - Assume we have sudo privaleges run running the virus. We can add all users to some group that can run the virus without sudo.
    - Can't listen to already listening ports.
    1. Custom interface
        - Forward all traffic to custom interface.
        - Would require a lot of work
    2. Iptables
        - Probably want to back this up before messing with it. `iptables-save > iptables-restore`
        - Need to forward packets to a listening process. Write this in Rust.
    3. Create custom dns resolver that forwards requests to custom server handler
        - Messing with iptables is lowerlevel and more flexible
    4. nfqueues
        - No. Seems to mostly be installed by default on RedHat distributions. Others one must install it first. Small population.

## netedit
Rust progam which iptables will forward new connections to.


### Steps
The first goal is to find the code that is writing the file, so we can insert virus code in there.
1.
```bash
$ strace -f gcc main.c -o build/m-static 2>&1 > /dev/null | grep m-static -C 5
[pid 1379643] readlink("/usr/lib/gcc/x86_64-pc-linux-gnu/12.2.1", 0x7ffc54f65390, 1023) = -1 EINVAL (Invalid argument)
[pid 1379643] faccessat2(AT_FDCWD, "/usr/lib/gcc/x86_64-pc-linux-gnu/12.2.1/", F_OK, AT_EACCESS) = 0
[pid 1379643] readlink("/usr/lib", 0x7ffc54f65390, 1023) = -1 EINVAL (Invalid argument)
[pid 1379643] readlink("/usr/lib/crtn.o", 0x7ffc54f65390, 1023) = -1 EINVAL (Invalid argument)
[pid 1379643] prlimit64(0, RLIMIT_NOFILE, NULL, {rlim_cur=1024, rlim_max=512*1024}) = 0
[pid 1379643] newfstatat(AT_FDCWD, "build/m-static", {st_mode=S_IFREG|0755, st_size=15232, ...}, 0) = 0
[pid 1379643] newfstatat(AT_FDCWD, "build/m-static", {st_mode=S_IFREG|0755, st_size=15232, ...}, AT_SYMLINK_NOFOLLOW) = 0
[pid 1379643] unlink("build/m-static")  = 0
[pid 1379643] openat(AT_FDCWD, "build/m-static", O_RDWR|O_CREAT|O_TRUNC, 0666) = 3
[pid 1379643] fcntl(3, F_GETFD)         = 0
[pid 1379643] fcntl(3, F_SETFD, FD_CLOEXEC) = 0
[pid 1379643] openat(AT_FDCWD, "/usr/lib/gcc/x86_64-pc-linux-gnu/12.2.1/../../../../lib/Scrt1.o", O_RDONLY) = 4
[pid 1379643] fcntl(4, F_GETFD)         = 0
[pid 1379643] fcntl(4, F_SETFD, FD_CLOEXEC) = 0
--
[pid 1379643] read(3, "\177ELF\2\1\1\0\0\0\0\0\0\0\0\0\3\0>\0\1\0\0\0 \20\0\0\0\0\0\0"..., 4096) = 4096
[pid 1379643] munmap(0x7f06cb5e3000, 135168) = 0
[pid 1379643] lseek(3, -3208, SEEK_CUR) = 888
[pid 1379643] write(3, "\4\0\0\0\24\0\0\0\3\0\0\0GNU\0lI.\345`q\203\360Z\322F\326J\257D\370"..., 36) = 36
[pid 1379643] close(3)                  = 0
[pid 1379643] newfstatat(AT_FDCWD, "build/m-static", {st_mode=S_IFREG|0644, st_size=15232, ...}, 0) = 0
[pid 1379643] umask(000)                = 022
[pid 1379643] umask(022)                = 000
[pid 1379643] chmod("build/m-static", 0755) = 0
[pid 1379643] close(17)                 = 0
[pid 1379643] close(7)                  = 0
[pid 1379643] close(6)                  = 0
[pid 1379643] close(18)                 = 0
[pid 1379643] close(5)                  = 0
```

Piping this command to `grep -E 'write.*ELF` gives a single line, which

I use `-f` because I notice there must be more syscalls than I'm getting without the flag, and looking the help for strace, `-f` includes forked threads.
I want to follow system calls to see where `m-static` is being written. I saw a `execve()` syscall which looks like it is running a command, confirmed by its corresponding man page. Passing the command output to `grep write` gives no output.


2. `gdb`

Now that I know which syscall and its arguments is probably writing the file, I can use gdb to step through to see what exactly happening.
