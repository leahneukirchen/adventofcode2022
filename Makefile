all: $(patsubst %.zig,%.exe,$(wildcard *.zig))

day%.exe: day%.zig day%
	zig build-exe -O ReleaseSafe $< --name $@

clean: FRC
	rm -f *.exe

FRC:
