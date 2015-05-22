# TCFS-ERL

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
git clone https://github.com/hmgle/tcfs-erl.git
cd tcfs
mkdir mountpoint
make
./tcfs mountpoint
ls -shal mountpoint
```
