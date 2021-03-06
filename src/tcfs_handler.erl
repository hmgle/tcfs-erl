-module(tcfs_handler).
-compile(export_all).

-include_lib("kernel/include/file.hrl").

-define(GETATTR, 1:32).
-define(READLINK, 2:32).
-define(GETDIR, 3:32).
-define(MKNOD, 4:32).
-define(MKDIR, 5:32).
-define(SYMLINK, 6:32).
-define(UNLINK, 7:32).
-define(RMDIR, 8:32).
-define(RENAME, 9:32).
-define(CHMOD, 10:32).
-define(CHOWN, 11:32).
-define(TRUNCATE, 12:32).
-define(UTIME, 13:32).
-define(OPEN, 14:32).
-define(READ, 15:32).
-define(WRITE, 16:32).
-define(READDIR, 17:32).
-define(RELEASE, 18:32).
-define(CREATE, 19:32).

truncate(Path, Newsize) ->
    {file_exists, true} = {file_exists, filelib:is_file(Path)},
    {ok, IO} = file:open(Path, [read, write]),
    {ok, _NewPosition} = file:position(IO, Newsize),
    ok = file:truncate(IO),
    file:close(IO).

%% see http://www.epochconverter.com/
datetime_to_epoch_time(DateTime) ->
    %% 62167219200 = 719528*24*3600 =
    %%      calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.

%% FIXME: check FixPath is the subdir of RootPath
msg_handler([RootPath], <<?GETATTR, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    case file:read_file_info(FixPath) of
        %% TODO: get st_blksize, st_blocks
        {ok, FileInfo} ->
            %% error_logger:info_msg("fileinfo: ~p", [FileInfo]),
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
msg_handler([RootPath], <<?READDIR, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    case file:list_dir_all(FixPath) of
        {ok, Filenames} ->
            FileLists = lists:flatmap(fun(F) -> F ++ "\0" end, Filenames),
            Reply = [<<0:32>>, unicode:characters_to_binary(FileLists)],
            {ok, Reply};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-9:32>>} % EBADF = 9
    end;
msg_handler([RootPath], <<?OPEN, Flags:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    Modes = parse_open_modes(Flags),
    case file:open(FixPath, Modes) of
        {ok, F} ->
            FIndex = gen_fd_index(),
            put(FIndex, F),
            Reply = [<<0:32>>, <<FIndex:32>>],
            %% error_logger:info_msg("open Findex: ~p~n", [FIndex]),
            {ok, Reply};
        {error, _Reasor} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([_RootPath], <<?READ, FIndex:32, Offset:32, Size:32, _Path/binary>>) ->
    F = get(FIndex),
    case F of
        undefined ->
            {ok, <<-1:32>>};
        _ ->
            case file:pread(F, Offset, Size) of
                {ok, Data} ->
                    Readed = byte_size(Data),
                    Reply = [<<Readed:32>>, Data],
                    {ok, Reply};
                eof ->
                    {ok, <<0:32>>};
                {error, _Reason} ->
                    %% TODO: get and send errno
                    {ok, <<-9:32>>} % EBADF = 9
            end
    end;
msg_handler([_RootPath], <<?WRITE, FIndex:32, Offset:32, Size:32, Wbuf/binary>>) ->
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
msg_handler([RootPath], <<?TRUNCATE, Newsize:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    case ?MODULE:truncate(FixPath, Newsize) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([_RootPath], <<?RELEASE, FIndex:32>>) ->
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
msg_handler([RootPath], <<?MKDIR, _Mode:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    case file:make_dir(FixPath) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([RootPath], <<?CHMOD, Mode:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    case file:change_mode(FixPath, Mode) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([RootPath], <<?RMDIR, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    case file:del_dir(FixPath) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([RootPath], <<?UNLINK, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    case file:delete(FixPath) of
        ok ->
            {ok, <<0:32>>};
        {error, _Reason} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([RootPath], <<?CREATE, _Mode:32, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
    case file:open(FixPath, [raw, write, binary]) of
        {ok, F} ->
            FIndex = gen_fd_index(),
            put(FIndex, F),
            Reply = [<<0:32>>, <<FIndex:32>>],
            %% error_logger:info_msg("create Findex: ~p~n", [FIndex]),
            {ok, Reply};
        {error, _Reasor} ->
            %% TODO: get and send errno
            {ok, <<-13:32>>} % EACCES = 13
    end;
msg_handler([RootPath], <<?UTIME, _Actime:64, _Modtime:64, Path/binary>>) ->
    FixPath = atom_to_list(RootPath) ++ unicode:characters_to_list(Path),
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
