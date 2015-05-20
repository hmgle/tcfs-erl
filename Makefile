all::
	erl -pa ebin -make

clean::
	-rm -f ebin/*.beam

run: all
	erl -pa ebin -s tcfs_server
