.PHONY: run

bin/program.exe: OptionExtensions.fs Constants.fs NeighbourHood.fs Graph.fs Block.fsi Block.fs Lattice.fsi Lattice.fs Program.fs
	fsharpc $^ -o $@

run: bin/program.exe
	mono $<
