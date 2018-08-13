src := $(wildcard src/*.c)
out := $(src:src/%.c=out/%.o)

inc_path := inc/
lib_path := lib/

bin := nihil.exe
def := DEBUG
opt := -std=c11 -g -Og -c -Wall
lib :=

out/%.o: src/%.c
	gcc $(opt) $(def:%=-D%) $< -o $@ -I$(inc_path)

$(bin): $(out)
	gcc $^ -o $@ $(lib:%=-l%) -L$(lib_path)

clean:
	rm out/*
	rm $(bin)