-module(tcfs_handler).
-compile(export_all).

-include_lib("kernel/include/file.hrl").

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
    {file_exists, true} = {file_exists, filelib:is_file(Path)},
    {ok, IO} = file:open(Path, [read, write]),
    {ok, _NewPosition} = file:position(IO, Newsize),
    ok = file:truncate(IO),
    file:close(IO).

utime(Path, Mtime) ->
    file:change_time(Path, Mtime).

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

%% FIXME: check FixPath is the subdir of RootPath
msg_handler([RootPath], <<"getattr", Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    case file:read_file_info(FixPath) of
        %% TODO: get st_blksize, st_blocks
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
            %% TODO: get and send errno
            {ok, <<-2:32>>} % ENOENT = 2
    end;
msg_handler([RootPath], <<"readdir", Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    case file:list_dir_all(FixPath) of
        {ok, Filenames} ->
            FileLists = lists:flatmap(fun(F) -> F ++ "\0" end, Filenames),
            Reply = [<<0:32>>, FileLists],
            {ok, Reply};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-9:32>>} % EBADF = 9
    end;
msg_handler([RootPath], <<"open", Flags:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    Modes = parse_open_modes(Flags),
    case file:open(FixPath, Modes) of
        {ok, F} ->
            FIndex = gen_fd_index(),
            put(FIndex, F),
            Reply = [<<0:32>>, <<FIndex:32>>],
            error_logger:info_msg("open Findex: ~p~n", [FIndex]),
            {ok, Reply};
        {error, _Reasor} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([_RootPath], <<"read", FIndex:32, Offset:32, Size:32, _Path/binary>>) ->
    F = get(FIndex),
    case F of
        undefined ->
            {ok, <<-1:32>>};
        _ ->
            case file:pread(F, Offset, Size) of
                {ok, Data} ->
                    Reply = [<<Size:32>>, Data],
                    {ok, Reply};
                eof ->
                    {ok, <<0:32>>};
                {error, _Reason} ->
                    %% TODO: get and send errno
                    {ok, <<-11:32>>} % EAGAIN = 11
            end
    end;
msg_handler([_RootPath], <<"write", FIndex:32, Offset:32, Size:32, Wbuf/binary>>) ->
    F = get(FIndex),
    case F of
        undefined ->
            {ok, <<-1:32>>};
        _ ->
            case file:pwrite(F, Offset, Wbuf) of
                ok ->
                    {ok, <<Size:32>>};
                {error, _Reason} ->
                    %% TODO: get and send errno
                    {ok, <<-11:32>>} % EAGAIN = 11
            end
    end;
msg_handler([RootPath], <<"truncate", Newsize:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    case ?MODULE:truncate(FixPath, Newsize) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([_RootPath], <<"release", FIndex:32>>) ->
    F = get(FIndex),
    case F of
        undefined ->
            {ok, <<-9:32>>}; % EBADF = 9
        _ ->
            case file:close(F) of
                ok ->
                    {ok, <<0:32>>};
                {error, _Reason} ->
                    %% TODO: get and send errno
                    {ok, <<-9:32>>} % EBADF = 9
            end
    end;
msg_handler([RootPath], <<"mkdir", _Mode:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    case file:make_dir(FixPath) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([RootPath], <<"rmdir", Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    case file:del_dir(FixPath) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([RootPath], <<"create", _Mode:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    case file:open(FixPath, [raw, write, binary]) of
        {ok, F} ->
            FIndex = gen_fd_index(),
            put(FIndex, F),
            Reply = [<<0:32>>, <<FIndex:32>>],
            error_logger:info_msg("create Findex: ~p~n", [FIndex]),
            {ok, Reply};
        {error, _Reasor} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([RootPath], <<"utime", _Actime:64, _Modtime:64, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ binary_to_list(Path),
    %% TODO: parse Actime and Modtime
    case file:change_time(FixPath, erlang:localtime(), erlang:localtime()) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reasor} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler(_RootPath, _) ->
    {error, badmsg}.

gen_fd_index() ->
    Ref = erlang:ref_to_list(erlang:make_ref()),
    {match, RefList} = re:run(Ref, "\\d+", [global, {capture, all, binary}]),
    [[_], [_], [_], [Ret]] = RefList,
    erlang:binary_to_integer(Ret).

bit(Number, Bit) ->
    (Number bsr Bit) band 1.

parse_open_modes(Flags) ->
    %% TODO
    RDONLY_or_WRONLY = ?MODULE:bit(Flags, 0),
    IsRDWR = ?MODULE:bit(Flags, 1),
    _IsCREAT = ?MODULE:bit(Flags, 6),
    _IsTRUNC = ?MODULE:bit(Flags, 9),
    _IsAPPEND = ?MODULE:bit(Flags, 10),
    Modes = [binary, raw],
    case RDONLY_or_WRONLY of
        1 -> [write | Modes];
        0 ->
            case IsRDWR of
                1 -> [read, write | Modes];
                0 -> Modes
            end
    end.
