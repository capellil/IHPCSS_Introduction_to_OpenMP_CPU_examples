CC = gcc
CFLAGS += -O3 -fopenmp
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/whoseturn

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/whoseturn: $(SRC_DIR)/whoseturn.c
	$(CC) -o $@ $< $(CFLAGS)
