.PHONY: run repl

bin/program.exe: OptionExtensions.fs Constants.fs NeighbourHood.fs Graph.fs Block.fsi Block.fs Lattice.fsi Lattice.fs StructureGraph.fs Program.fs
	fsharpc $^ -o $@

run: bin/program.exe
	mono $<


repl: OptionExtensions.fs Constants.fs NeighbourHood.fs Graph.fs Block.fs Lattice.fs StructureGraph.fs
	fsharpi $^
