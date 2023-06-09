CC = gcc
CFLAGS += -Wall -Wextra -O3 -fopenmp
SRC_DIR=src
BIN_DIR=bin

default: all

all: $(BIN_DIR)/preamble

$(BIN_DIR)/preamble: $(SRC_DIR)/preamble.c
	$(CC) -o $@ $< $(CFLAGS)
