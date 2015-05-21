-module(tcfs_handler).
-compile(export_all).

-include_lib("kernel/include/file.hrl").

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

%% see http://www.epochconverter.com/
datetime_to_epoch_time(DateTime) ->
    %% 62167219200 = 719528*24*3600 =
    %%      calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.

%% msg_handler(Msg) ->
msg_handler([RootPath], <<"getattr", Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    case file:read_file_info(FixPath) of
        {ok, FileInfo} ->
            error_logger:info_msg("fileinfo: ~p", [FileInfo]),
            #file_info{major_device=Dev, inode=Inode, mode=Mode, links=Nlink,
                      uid=Uid, gid=Gid, size=Size, atime=Atime, mtime=Mtime,
                      ctime=Ctime} = FileInfo,
            Reply = <<0:32, Dev:32, Inode:32, Mode:32, Nlink:32, Uid:32, Gid:32,
                      Size:32, (?MODULE:datetime_to_epoch_time(Atime)):32,
                      (?MODULE:datetime_to_epoch_time(Mtime)):32,
                      (?MODULE:datetime_to_epoch_time(Ctime)):32>>,
            {ok, Reply};
        {error, _Reason} ->
            Reply = <<1:32, 0:320>>,
            {ok, Reply}
    end;
msg_handler(_RootPath, _) ->
    {error, badmsg}.

