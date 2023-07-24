#!/usr/bin/env python3

from pwn import *

{bindings}

context.binary = {bin_name}
context.terminal = ['tmux', 'splitw', '-h']


def conn():
    if args.REMOTE:
        io = remote("addr", 1337)
    else:
        if args.GDB:
            io = gdb.debug({proc_args}, aslr=False, gdbscript="""
                    set follow-fork-mode parent
               """)
        else:
            io = process({proc_args})
            #gdb.attach(io)
    return io


def main():
    io = conn()
    io.interactive()


if __name__ == "__main__":
    main()
