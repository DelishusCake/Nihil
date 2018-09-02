src := $(wildcard src/*.c)
out := $(src:src/%.c=out/%.o)

lib := $(wildcard lib/*.c)
lib_out := $(lib:lib/%.c=out/%.o)

inc_path := lib/

bin := nihil.exe
def := DEBUG
opt := -std=c11 -g -Og -c -Wall
lib :=

out/%.o: src/%.c 
	gcc $(opt) $(def:%=-D%) $^ -o $@ -I$(inc_path)
out/%.o: lib/%.c 
	gcc $(opt) $(def:%=-D%) $^ -o $@ -I$(inc_path)

$(bin): $(out) $(lib_out)
	gcc $^ -o $@ $(lib:%=-l%)

clean:
	rm out/*
	rm $(bin)