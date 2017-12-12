.PHONY: run repl

bin/program.exe: SeqExtensions.fs OptionExtensions.fs Constants.fs Graph.fs NeighbourHood.fs Block.fsi Block.fs Lattice.fsi Lattice.fs StructureGraph.fs Evolution.fs GraphEvolve.fs Program.fs
	fsharpc --debug:full $^ -o $@

run: bin/program.exe
	mono --debug $<


repl: SeqExtensions.fs OptionExtensions.fs Constants.fs Graph.fs NeighbourHood.fs Block.fsi Block.fs Lattice.fsi Lattice.fs StructureGraph.fs Evolution.fs GraphEvolve.fs
	fsharpi $^
