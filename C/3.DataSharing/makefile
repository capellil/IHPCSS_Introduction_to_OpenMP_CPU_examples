CC = gcc
CFLAGS += -O3 -fopenmp
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/datasharing

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/datasharing: $(SRC_DIR)/datasharing.c
	$(CC) -o $@ $< $(CFLAGS)
