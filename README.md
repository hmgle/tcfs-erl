# TCFS-ERL

[![Build Status](https://travis-ci.org/hmgle/tcfs-erl.png?branch=master)](https://travis-ci.org/hmgle/tcfs-erl)

TCFS, a Toy Cloud File System.

Tcfs-erl is the tcfs server writen in Erlang.

## How to use?

- Server:

```
git clone https://github.com/hmgle/tcfs-erl.git
cd tcfs-erl
make
make run
```

- Client:

```
sudo apt-get install libfuse-dev
git clone https://github.com/hmgle/tcfs.git
cd tcfs
mkdir mountpoint
make
./tcfs --server 127.0.0.1 mountpoint
ls -shal mountpoint
```
