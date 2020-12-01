
io: erlang/erlang_io.erl
	erlc +debug_info erlang/erlang_io.erl
	./erlscripten -s erlang_io.beam -o src/Erlang/IO.purs
	rm erlang_io.beam
