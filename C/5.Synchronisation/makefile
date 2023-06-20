CC = gcc-13
CFLAGS += -O3 -fopenmp
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/synchronisation

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/synchronisation: $(SRC_DIR)/synchronisation.c
	$(CC) -o $@ $< $(CFLAGS)
