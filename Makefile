all:
	make contrib_r
	mkdir -p ebin
	erlc -pa contrib/uberpt/ebin -o ebin -I include src/*.erl

contrib_r:
	$(MAKE) -C contrib
