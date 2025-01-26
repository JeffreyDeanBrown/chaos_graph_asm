chaosgraph: chaosgraph.o
	ld -o chaosgraph chaosgraph.o -m elf_i386
chaosgraph.o: chaosgraph.asm
	nasm -f elf -g -F dwarf chaosgraph.asm -l chaosgraph.lst
