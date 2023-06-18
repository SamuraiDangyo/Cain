# Cain. Linux UCI SmallChess engine. Written in C++20 language
# Copyright (C) 2023 Toni Helminen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Definitions

CXX    = clang++
EXE    = cain
BFLAGS = -std=c++20 -flto -O3 -march=native -DNDEBUG
WFLAGS = -Wall -Wextra -Wshadow -Wcast-qual -pedantic

# Targets

all:
	$(CXX) $(BFLAGS) $(WFLAGS) $(CXXFLAGS) -o $(EXE) main.cpp

clean:
	rm -f $(EXE)

help:
	@echo '+-+ How To Compile Cain +-+'
	@echo '-DWINDOWS                       # Windows Build'
	@echo '(CXX(FLAGS)|EXE|[BWN]FLAGS)=... # Build Flags'
	@echo 'make -j                         # > Simple Build'

.PHONY: all clean help
