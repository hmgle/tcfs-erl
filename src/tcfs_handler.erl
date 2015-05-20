-module(tcfs_handler).
-compile(export_all).

getattr(Path) ->
    file:read_file_info(Path).

readlink(Path) ->
    file:read_link_all(Path).

mkdir(Path, _Mode) ->
    file:make_dir(Path).

un_link(_Path) ->
    {todo}.

rmdir(Path) ->
    file:del_dir(Path).

symlink(Path, Link) ->
    file:make_symlink(Path, Link).

rename(Oldpath, Newpath) ->
    file:rename(Oldpath, Newpath).

%% Hard link
make_link(Oldpath, Newpath) ->
    file:make_link(Oldpath, Newpath).

chmod(Path, Mode) ->
    file:change_mode(Path, Mode).

chown(Path, Uid, Gid) ->
    file:change_owner(Path, Uid, Gid).

truncate(Path, Newsize) ->
    %% FIXME
    {file_exists, true} = {file_exists, filelib:is_file(Path)},
    {ok, IO} = file:open(Path, [read, write]),
    {ok, Max} = file:position(IO, eof),
    {correct_size, true} = {correct_size, (Newsize < Max)},
    {ok, Newsize} = file:position(IO, {bof, Newsize}),
    ok = file:truncate(IO),
    file:close(IO).

utime(Path, Mtime) ->
    file:change_time(Path, Mtime).

open(_Path, _Mode) ->
    ok.

read(Path, Offset, Size) ->
    {ok, F} = file:open(Path, [read, binary, raw]),
    {ok, _Data} = file:pread(F, Offset, Size),
    file:close(F).

write(Path, Offset, _Size, Data) ->
    {ok, F} = file:open(Path, [raw, write, binary]),
    file:pwrite(F, Offset, Data),
    file:close(F).

readdir(Path) ->
    file:list_dir_all(Path).

