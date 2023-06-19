/*
Cain. Linux UCI SmallChess engine. Written in C++20 language
Copyright (C) 2020-2023 Toni Helminen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

// Header guard

#pragma once

// Headers

#include <bits/stdc++.h>
#include <unistd.h>

// Namespace

namespace cain {

// Constants

const std::string kVersion            = "Cain 0.2"; // Program version
const std::string kStartPos           = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w 0 1"; // UCI startpos
constexpr std::size_t kMaxMoves       = 256;      // Max chess moves
constexpr std::size_t kHashMB         = 256;      // MB
constexpr std::size_t kNoise          = 2;        // Noise for opening moves
constexpr std::size_t kMoveOverhead   = 100;      // ms
constexpr std::size_t kRepsDraw       = 2;        // 2 reps is draw
constexpr std::size_t kFifty          = 100;      // 100? moves is a draw (256 max)
constexpr std::size_t kReadClockTicks = 0x1FFULL; // Read clock every 512 ticks (white / 2 x both)
constexpr std::size_t kWeek           = (7 * 24 * 60 * 60 * 1000); // ms
constexpr int kMaxDepth               = 64;       // Max search depth (stack frame problems ...)
constexpr int kMaxQDepth              = 16;       // Max Qsearch depth
constexpr int kInf                    = 1048576;  // System max number
constexpr int kMaxPieces              = (2 * (8 * 1 + 2 * 3 + 2 * 3 + 2 * 5 + 1 * 9 + 1 * 0)); // Max pieces on board (Kings always exist)
constexpr int kFrcPenalty             = 100; // Blocked bishop by a pawn penalty
constexpr int kTempoBonus             = 25;  // Bonus for tempo
constexpr int kChecksBonus            = 17;  // Bonus for checks

// Tactical fens to pressure search
const std::vector<std::string> kBench = {
  "R7/P4k2/8/8/8/8/r7/6K1 w 0 1 ; bm a8b8",
  "2kr3r/pp1q1ppp/5n2/1Nb5/2Pp1B2/7Q/P4PPP/1R3RK1 w 0 1 ; bm h3g5",
  "2R5/2R4p/5p1k/6n1/8/1P2QPPq/r7/6K1 w 0 1 ; bm g1f1",
  "5n2/pRrk2p1/P4p1p/4p3/3N4/5P2/6PP/6K1 w 0 1 ; bm d4b5",
  "8/6pp/4p3/1p1n4/1NbkN1P1/P4P1P/1PR3K1/r7 w 0 1 ; bm e4d6",
  "2r5/2rk2pp/1pn1pb2/pN1p4/P2P4/1N2B3/nPR1KPPP/3R4 b 0 1 ; bm a2b4",
  "nrq4r/2k1p3/1p1pPnp1/pRpP1p2/P1P2P2/2P1BB2/1R2Q1P1/6K1 w 0 1 ; bm e2g3",
  "1k1r4/pp1r1pp1/4n1p1/2R5/2Pp1qP1/3P2QP/P4PB1/1R4K1 w 0 1 ; bm g3f4",
  "2r1k3/6pr/p1nBP3/1p3p1p/2q5/2P5/P1R4P/K2Q2R1 w 0 1 ; bm d6c7",
  "2b4k/p1b2p2/2p2q2/3p1PNp/3P2R1/3B4/P1Q2PKP/4r3 w 0 1 ; bm g5f7",
  "5bk1/1rQ4p/5pp1/2pP4/3n1PP1/7P/1q3BB1/4R1K1 w 0 1 ; bm c7e8",
  "rnbqkb1r/pppp1ppp/8/4P3/6n1/7P/PPPNPPP1/R1BQKBNR b 0 1 ; bm g4e5"
};

// [Attacker][Captured] / [PNBRQK][pnbrqk]
constexpr int kMvv[6][6] = {
  { 10, 15, 15, 20, 25, 99 }, { 9, 14, 14, 19, 24, 99 }, { 9, 14, 14, 19, 24, 99 },
  {  8, 13, 13, 18, 23, 99 }, { 7, 12, 12, 17, 22, 99 }, { 6, 11, 11, 16, 21, 99 }
};

// Evaluation phases      ( P  N  B  R  Q  K )
constexpr int kPiece[6] = { 1, 3, 3, 5, 9, 0 }; // Must match kMaxPieces !

// ( MG  EG ) -> ( P  N  B  R  Q  K )
constexpr int kPestoMaterial[2][6] = {
  { 82, 337, 365, 477, 1025, 0 },
  { 94, 281, 297, 512,  936, 0 }
};

// [Piece][Phase][Square]
constexpr int kHalfSquare[64] = {
  0,    1,   2,   3,   3,   2,   1,   0,
  4,    5,   6,   7,   7,   6,   5,   4,
  8,    9,  10,  11,  11,  10,   9,   8,
  12,  13,  14,  15,  15,  14,  13,  12,
  16,  17,  18,  19,  19,  18,  17,  16,
  20,  21,  22,  23,  23,  22,  21,  20,
  24,  25,  26,  27,  27,  26,  25,  24,
  28,  29,  30,  31,  31,  30,  29,  28
};

constexpr int kPestoPsqt[6][2][32] = {
{{ -55, -54, -53, -52, -35,  -1, -20, -23, -26,  -4,  -4, -10, -27,  -2,  -5,  12,
   -14,  13,   6,  21, -6,   7,  26,  31, 98, 134,  61,  95, 0,   0,   0,   0 },
 { -55, -54, -53, -52, 13,   8,   8,  10, 4,   7,  -6,   1, 13,   9,  -3,  -7,
    32,  24,  13,   5, 94, 100,  85,  67, 178, 173, 158, 134, 0,   0,   0,   0 }},
{{-105, -21, -58, -33, -29, -53, -12,  -3, -23,  -9,  12,  10, -13,   4,  16,  13,
    -9,  17,  19,  53, -47,  60,  37,  65, -73, -41,  72,  36, -167, -89, -34, -49 },
 { -29, -51, -23, -15, -42, -20, -10,  -5, -23,  -3,  -1,  15, -18,  -6,  16,  25,
   -17,   3,  22,  22, -24, -20,  10,   9, -25,  -8, -25,  -2, -58, -38, -13, -28 }},
{{ -33,  -3, -14, -21, 4,  15,  16,   0, 0,  15,  15,  15, -6,  13,  13,  26,
    -4,   5,  19,  50, -16,  37,  43,  40, -26,  16, -18, -13, -29,   4, -82, -37 },
 { -23,  -9, -23,  -5, -14, -18,  -7,  -1, -12,  -3,   8,  10, -6,   3,  13,  19,
    -3,   9,  12,   9, 2,  -8,   0,  -1, -8,  -4,   7, -12, -14, -21, -11,  -8 }},
{{ -19, -13,   1,  17, -44, -16, -20,  -9, -45, -25, -16, -17, -36, -26, -12,  -1,
   -24, -11,   7,  26, -5,  19,  26,  36, 27,  32,  58,  62, 32,  42,  32,  51 },
  { -9,   2,   3,  -1, -6,  -6,   0,   2, -4,   0,  -5,  -1, 3,   5,   8,   4,
     4,   3,  13,   1, 7,   7,   7,   5, 11,  13,  13,  11, 13,  10,  18,  15 }},
{{  -1, -18,  -9,  10, -35,  -8,  11,   2, -14,   2, -11,  -2, -9, -26,  -9, -10,
   -27, -27, -16, -16, -13, -17,   7,   8, -24, -39,  -5,   1, -28,   0,  29,  12 },
 { -33, -28, -22, -43, -22, -23, -30, -16, -16, -27,  15,   6, -18,  28,  19,  47,
     3,  22,  24,  45, -20,   6,   9,  49, -17,  20,  32,  41, -9,  22,  22,  27 }},
{{ -15,  36,  12, -54, 1,   7,  -8, -64, -14, -14, -22, -46, -49,  -1, -27, -39,
   -17, -20, -12, -27, -9,  24,   2, -16, 29,  -1, -20,  -7, -65,  23,  16, -15 },
 { -53, -34, -21, -11, -27, -11,   4,  13, -19,  -3,  11,  21, -18,  -4,  21,  24,
    -8,  22,  24,  27, 10,  17,  23,  15, -12,  17,  14,  17, -74, -35, -18, -18 }}
};

// Enums

enum class MoveType { kKiller, kGood };

// Structs

struct Board { // 172B
  std::uint64_t white[6]{};   // White bitboards
  std::uint64_t black[6]{};   // Black bitboards
  std::int32_t  score{0};     // Sorting score
  std::int8_t   pieces[64]{}; // Pieces white and black
  std::uint8_t  index{0};     // Sorting index
  std::uint8_t  from{0};      // From square
  std::uint8_t  to{0};        // To square
  std::uint8_t  type{0};      // Move type ( 8:=q )
  std::uint8_t  fifty{0};     // Rule 50 counter ( 256 max )

  const std::string movename() const;
  const std::string to_fen() const;
  const std::string to_s() const;
};

struct HashEntry { // 10B
  std::uint32_t killer_hash{0}; // Killer move hash
  std::uint32_t good_hash{0};   // Good move hash
  std::uint8_t  killer{0};      // Killer move index
  std::uint8_t  good{0};        // Good move index

  template <MoveType> void update(const std::uint64_t, const std::uint8_t);
  void put_hash_value_to_moves(const std::uint64_t, Board*) const;
};

// Variables

std::uint64_t g_black = 0, g_white = 0, g_both = 0, g_empty = 0, g_good = 0, g_stop_search_time = 0,
  g_nodes = 0, g_pawn_sq = 0, g_pawn_1_moves_w[64]{}, g_pawn_1_moves_b[64]{}, g_knight_moves[64]{},
  g_king_moves[64]{}, g_bishop_moves[64]{}, g_rook_moves[64]{}, g_queen_moves[64]{}, g_pawn_checks_w[64]{},
  g_pawn_checks_b[64]{}, g_zobrist_wtm[2]{}, g_r50_positions[kFifty + 1]{}, g_zobrist_board[13][64]{};

int g_move_overhead = kMoveOverhead, g_level = 100, g_root_n = 0, g_moves_n = 0, g_max_depth = kMaxDepth,
  g_q_depth = 0, g_depth = 0, g_best_score = 0, g_noise = kNoise, g_last_eval = 0, g_fullmoves = 1;

bool g_wtm = false, g_nullmove_active = false, g_stop_search = false, g_is_pv = false,
     g_game_on = true, g_analyzing = false;

Board g_board_empty{}, *g_board = &g_board_empty, *g_moves = nullptr, *g_board_orig = nullptr,
  g_boards[kMaxDepth + kMaxQDepth][kMaxMoves]{};

std::uint32_t g_hash_entries = 0, g_tokens_nth = 0;
std::vector<std::string> g_tokens(300); // 300 plys init
std::unique_ptr<HashEntry[]> g_hash{};

// Prototypes

int SearchW(int, const int, const int, const int);
int SearchB(const int, int, const int, const int);
int QSearchB(const int, int, const int, const int);
int Evaluate(const bool);
bool ChecksW();
bool ChecksB();

// Utils

// White bitboards
inline std::uint64_t White() {
  return g_board->white[0] | g_board->white[1] | g_board->white[2] |
         g_board->white[3] | g_board->white[4] | g_board->white[5];
}

// Black bitboards
inline std::uint64_t Black() {
  return g_board->black[0] | g_board->black[1] | g_board->black[2] |
         g_board->black[3] | g_board->black[4] | g_board->black[5];
}

// Both colors
inline std::uint64_t Both() {
  return White() | Black();
}

// Set bit in 1 -> 64
constexpr std::uint64_t Bit(const int nth) {
  return 0x1ULL << nth;
}

// Count Trailing zeros AND then pop BitBoard
inline int CtzPop(std::uint64_t *b) {
  const auto ret = std::countr_zero(*b); // 010110100 -> 2
  *b &= *b - 0x1ULL;
  return ret;
}

// X axle of board
inline int Xaxl(const int sq) {
  return sq % 8;
}

// Y axle of board
inline int Yaxl(const int sq) {
  return sq / 8;
}

// Nodes Per Second
std::uint64_t Nps(const std::uint64_t nodes, const std::uint64_t ms) {
  return std::uint64_t(1000 * nodes) / std::max<std::uint64_t>(1, ms);
}

// Is (x, y) on board ? Slow, but only for init
bool OnBoard(const int x, const int y) {
  return x >= 0 && x <= 7 && y >= 0 && y <= 7;
}

// X-coord to char
char File2Char(const int f) {
  return 'a' + f;
}

// Y-coord to char
char Rank2Char(const int r) {
  return '1' + r;
}

// Convert int coords to string
const std::string Move2Str(const int from, const int to) {
  return std::string{File2Char(Xaxl(from)), Rank2Char(Yaxl(from)), File2Char(Xaxl(to)), Rank2Char(Yaxl(to))};
}

bool InputAvailable() { // See if std::cin has smt
  fd_set fd{};
  struct timeval tv = { .tv_sec = 0, .tv_usec = 0 };
  FD_ZERO(&fd);
  FD_SET(STDIN_FILENO, &fd);
  select(STDIN_FILENO + 1, &fd, nullptr, nullptr, &tv);
  return FD_ISSET(STDIN_FILENO, &fd) > 0;
}

// ms since 1.1.1970
inline std::uint64_t Now() {
  return std::chrono::duration_cast<std::chrono::milliseconds>(
      std::chrono::system_clock::now().time_since_epoch()) .count();
}

std::uint64_t Mixer(const std::uint64_t num) {
  return (num << 7) ^ (num >> 5);
}

// Deterministic Random()
std::uint64_t Random64() {
  static std::uint64_t a = 0X12311227ULL, b = 0X1931311ULL, c = 0X13138141ULL;
  a ^= b + c;
  b ^= b * c + 0x1717711ULL;
  c  = (3 * c) + 0x1ULL;
  return Mixer(a) ^ Mixer(b) ^ Mixer(c);
}

std::uint64_t Random8x64() { // 8x deterministic random for zobrist
  std::uint64_t ret = 0;
  for (auto i = 0; i < 8; ++i) ret ^= Random64() << (8 * i);
  return ret;
}

// Nondeterministic Rand()
int Random(const int min, const int max) {
  if (min == max) return 0;
  static std::uint64_t seed = 0x202c7ULL + std::uint64_t(std::time(nullptr));
  seed = (seed << 5) ^ (seed + 1) ^ (seed >> 3);
  return min + int(seed % std::max<std::uint64_t>(1, std::abs(max - min) + 1));
}

// Split string by given str
template <class T>
void SplitString(const std::string &str, T &cont, const std::string &delims = " ") {
  std::size_t cur = str.find_first_of(delims), prev = 0;
  while (cur != std::string::npos) {
    cont.push_back(str.substr(prev, cur - prev));
    prev = cur + 1;
    cur  = str.find_first_of(delims, prev);
  }
  cont.push_back(str.substr(prev, cur - prev));
}

// Read input from std::cin
void ReadInput() {
  std::string line{};
  std::getline(std::cin, line);
  g_tokens_nth = 0;
  g_tokens.clear();
  SplitString< std::vector<std::string> >(line, g_tokens);
}

// Hashtable

void SetHashtable(int hash_mb) {
  hash_mb = std::clamp(hash_mb, 1, 1048576); // Limits 1MB -> 1TB
  g_hash_entries = std::uint32_t((1 << 20) * hash_mb) / (sizeof(HashEntry)); // Hash(B) / Block(B)
  g_hash.reset(new HashEntry[g_hash_entries]); // Claim space
}

// Hash

inline std::uint64_t Hash(const bool wtm) {
  auto ret = g_zobrist_wtm[wtm];
  for (auto both = Both(); both; ) {
    const auto sq = CtzPop(&both);
    ret ^= g_zobrist_board[g_board->pieces[sq] + 6][sq];
  }
  return ret;
}

// HashEntry

// Update hashtable sorting algorithm
template <MoveType type>
void HashEntry::update(const std::uint64_t hash, const std::uint8_t index) {
  if constexpr (type == MoveType::kKiller) {
    this->killer_hash = std::uint32_t(hash >> 32);
    this->killer      = index + 1;
  } else { // == MoveType::kGood !
    this->good_hash = std::uint32_t(hash >> 32);
    this->good      = index + 1;
  }
}

// Best moves put first for maximum cutoffs
void HashEntry::put_hash_value_to_moves(const std::uint64_t hash, Board *moves) const {
  if (this->killer && (this->killer_hash == std::uint32_t(hash >> 32)))
    moves[this->killer - 1].score += 10000;
  if (this->good && (this->good_hash == std::uint32_t(hash >> 32)))
    moves[this->good - 1].score += 7000;
}

// Board

const std::string Board::movename() const {
  auto from2 = this->from, to2 = this->to;
  switch (this->type) {
    case 8: return Move2Str(from2, to2) + 'q';
  }
  return Move2Str(from2, to2);
}

// Board presentation in FEN ( Forsyth–Edwards Notation )
const std::string Board::to_fen() const {
  std::stringstream s{};
  for (auto r = 7; r >= 0; --r) {
    auto empty = 0;
    for (auto f = 0; f <= 7; ++f)
      if (const auto p = "kqrbnp.PNBRQK"[this->pieces[8 * r + f] + 6]; p == '.') {
        ++empty;
      } else {
        if (empty) s << empty, empty = 0;
        s << p;
      }
    if (empty)  s << empty;
    if (r != 0) s << "/";
  }
  s << (g_wtm ? " w " : " b ");
  s << int(this->fifty) << " " << int(std::max(1, g_fullmoves));
  return s.str();
}

// String presentation of board
const std::string Board::to_s() const {
  std::stringstream s{};
  s << " +---+---+---+---+---+---+---+---+\n";
  for (auto r = 7; r >= 0; --r) {
    for (auto f = 0; f <= 7; ++f)
      s << " | " << "kqrbnp PNBRQK"[this->pieces[8 * r + f] + 6];
    s << " | " << (1 + r) << "\n +---+---+---+---+---+---+---+---+\n";
  }
  s << "   a   b   c   d   e   f   g   h\n\n" <<
    "> " << this->to_fen() << '\n' <<
    "> Eval: " << std::showpos << Evaluate(g_wtm) << std::noshowpos;
  return s.str();
}

// Tokenizer

bool TokenOk(const std::uint32_t nth = 0) {
  return g_tokens_nth + nth < g_tokens.size(); // O(1)
}

const std::string TokenNth(const std::uint32_t nth = 0) {
  return TokenOk(nth) ? g_tokens[g_tokens_nth + nth] : "";
}

void TokenPop(const std::uint32_t nth = 1) {
  g_tokens_nth += nth;
}

bool TokenPeek(const std::string &token, const std::uint32_t nth = 0) {
  return TokenOk(nth) && token == g_tokens[g_tokens_nth + nth];
}

// If true then pop n
bool Token(const std::string &token, const std::uint32_t pop_n = 1) {
  if (TokenPeek(token)) {
    TokenPop(pop_n);
    return true;
  }
  return false;
}

int TokenNumber(const std::uint32_t nth = 0) {
  return TokenOk(nth) ? std::stoi(g_tokens[g_tokens_nth + nth]) : 0;
}

// Fen handling

void PutPiece(const int sq, const int p) {
  // Put piece on board
  g_board->pieces[sq] = p;

  // Create bitboards
  if      (p > 0) g_board->white[+p - 1] |= Bit(sq);
  else if (p < 0) g_board->black[-p - 1] |= Bit(sq);
}

int Piece2Num(const char p) { // Convert piece (Char) -> Int
  switch (p) {
    case 'P': return +1;
    case 'N': return +2;
    case 'B': return +3;
    case 'R': return +4;
    case 'Q': return +5;
    case 'K': return +6;
    case 'p': return -1;
    case 'n': return -2;
    case 'b': return -3;
    case 'r': return -4;
    case 'q': return -5;
    case 'k': return -6;
    default:  return  0; // Impossible
  }
}

int Empty2Num(const char e) { return e - '0'; } // Empty cells (Char) -> Int

void FenBoard(const std::string &board) {
  auto sq = 56;
  for (std::size_t i = 0; i < board.length() && sq >= 0; ++i) // O(n)
    if (const auto c = board[i]; c == '/') sq -= 16;
    else if (std::isdigit(c))              sq += Empty2Num(c);
    else                                   PutPiece(sq++, Piece2Num(c));
}

void FenRule50(const std::string &fifty) {
  if (fifty.length() != 0 && fifty[0] != '-') g_board->fifty = std::clamp(std::stoi(fifty), 0, 100);
}

void FenFullMoves(const std::string &fullmoves) {
  if (fullmoves.length() != 0) g_fullmoves = std::max(std::stoi(fullmoves), 1);
}

// Fully legal FEN expected
void FenGen(std::string fen) {
  if (fen.length()) std::replace(fen.begin(), fen.end(), '_', ' '); // "_" -> " ": Little hack
  std::vector<std::string> tokens{};
  SplitString< std::vector<std::string> >(fen, tokens);
  if (fen.length() < std::string("8/8/8/8/8/8/8/8 w 0 1").size() ||
      tokens.size() < 4 ||
      tokens[0].find('K') == std::string::npos ||
      tokens[0].find('k') == std::string::npos)
    throw std::runtime_error("info string Bad fen: " + fen);

  FenBoard(tokens[0]);
  g_wtm = tokens[1][0] == 'w';
  FenRule50(tokens[2]);
  FenFullMoves(tokens[3]);
}

// Reset board
void FenReset() {
  g_board_empty = {};
  g_board       = &g_board_empty;
  g_wtm         = true;
  g_fullmoves   = 1;

  for (auto i = 0; i < 6; ++i) g_board->white[i] = g_board->black[i] = 0;
}

void SetFen(const std::string &fen) {
  FenReset();
  FenGen(fen);
}

// Checks

inline bool ChecksHereW(const int sq) {
  return (g_pawn_checks_b[sq]        & g_board->white[0]) |
         (g_knight_moves[sq]         & g_board->white[1]) |
         (g_bishop_moves[sq]         & g_board->white[2]) |
         (g_rook_moves[sq]           & g_board->white[3]) |
         (g_queen_moves[sq]          & g_board->white[4]) |
         (g_king_moves[sq]           & g_board->white[5]);
}

inline bool ChecksHereB(const int sq) {
  return (g_pawn_checks_w[sq]        & g_board->black[0]) |
         (g_knight_moves[sq]         & g_board->black[1]) |
         (g_bishop_moves[sq]         & g_board->black[2]) |
         (g_rook_moves[sq]           & g_board->black[3]) |
         (g_queen_moves[sq]          & g_board->black[4]) |
         (g_king_moves[sq]           & g_board->black[5]);
}

inline bool ChecksW() {
  return ChecksHereW(std::countr_zero(g_board->black[5]));
}

inline bool ChecksB() {
  return ChecksHereB(std::countr_zero(g_board->white[5]));
}

// Sorting

// Sort only one node at a time ( Avoid costly n! / tons of operations )
// Swap every node for simplicity ( See: lazy-sorting-algorithm paper )
inline void LazySort(const int ply, const int nth, const int total_moves) {
  for (auto i = nth + 1; i < total_moves; ++i)
    if (g_boards[ply][i].score > g_boards[ply][nth].score)
      std::swap(g_boards[ply][nth], g_boards[ply][i]);
}

// 1. Evaluate all root moves
void EvalRootMoves() {
  for (auto i = 0; i < g_root_n; ++i)
    g_board         = g_boards[0] + i, // Pointer to this board
    g_board->score += (g_board->type == 8 ? 1000 : 0) + // =q
                      (Random(-g_noise, +g_noise)) + // Add noise -> Make unpredictable
                      (g_wtm ? +1 : -1) * Evaluate(g_wtm); // Full eval
}

// 2. Then sort root moves
struct RootCompFunctor { bool operator()(const Board &a, const Board &b) const { return a.score > b.score; } };
void SortRootMoves() { std::sort(g_boards[0] + 0, g_boards[0] + g_root_n, RootCompFunctor()); } // 9 -> 0

void SortRoot(const int index) {
  if (index) {
    const auto tmp = g_boards[0][index];
    for (auto i = index; i > 0; --i)
      g_boards[0][i] = g_boards[0][i - 1];
    g_boards[0][0] = tmp;
  }
}

void SwapMoveInRootList(const int index) {
  if (index)
    std::swap(g_boards[0][0], g_boards[0][index]);
}

// Move generator

void ModifyPawnStuffW(const int to) {
  if (g_board->pieces[to] != +1) return;
  g_board->fifty = 0;
  if (Yaxl(to) == 6) g_board->score = 85 + Yaxl(to); // Bonus for 7th ranks
}

void ModifyPawnStuffB(const int to) {
  if (g_board->pieces[to] != -1) return;
  g_board->fifty = 0;
  if (Yaxl(to) == 1) g_board->score = 85 + 7 - Yaxl(to);
}

void AddPromotionW(const int from, const int to, const int piece) {
  const auto eat = g_board->pieces[to];

  g_moves[g_moves_n]         = *g_board;
  g_board                    = &g_moves[g_moves_n];
  g_board->from              = from;
  g_board->to                = to;
  g_board->score             = 110 + piece; // Highest priority
  g_board->type              = 8;
  g_board->fifty             = 0;
  g_board->pieces[to]        = piece;
  g_board->pieces[from]      = 0;
  g_board->white[0]         ^= Bit(from);
  g_board->white[piece - 1] |= Bit(to);

  if (eat <= -1)  g_board->black[-eat - 1] ^= Bit(to);
  if (!ChecksB()) g_board->index = g_moves_n++;
}

void AddPromotionB(const int from, const int to, const int piece) {
  const auto eat = g_board->pieces[to];

  g_moves[g_moves_n]          = *g_board;
  g_board                     = &g_moves[g_moves_n];
  g_board->from               = from;
  g_board->to                 = to;
  g_board->score              = 110 + (-piece);
  g_board->type               = 8;
  g_board->fifty              = 0;
  g_board->pieces[from]       = 0;
  g_board->pieces[to]         = piece;
  g_board->black[0]          ^= Bit(from);
  g_board->black[-piece - 1] |= Bit(to);

  if (eat >= +1)  g_board->white[eat - 1] ^= Bit(to);
  if (!ChecksW()) g_board->index = g_moves_n++;
}

void AddPromotionStuffW(const int from, const int to) {
  AddPromotionW(from, to, +5), g_board = g_board_orig; // QRBN
}

void AddPromotionStuffB(const int from, const int to) {
  AddPromotionB(from, to, -5), g_board = g_board_orig;
}

inline void CheckNormalCapturesW(const int me, const int eat, const int to) {
  if (eat <= -1) {
    g_board->black[-eat - 1] ^= Bit(to);
    g_board->score            = kMvv[me - 1][-eat - 1];
    g_board->fifty            = 0;
  }
}

inline void CheckNormalCapturesB(const int me, const int eat, const int to) {
  if (eat >= +1) {
    g_board->white[eat - 1] ^= Bit(to);
    g_board->score           = kMvv[-me - 1][eat - 1];
    g_board->fifty           = 0;
  }
}

inline void AddMoveIfOkW() {
  if (!ChecksB()) g_board->index = g_moves_n++;
}

inline void AddMoveIfOkB() {
  if (!ChecksW()) g_board->index = g_moves_n++;
}

void AddNormalStuffW(const int from, const int to) {
  const auto me = g_board->pieces[from], eat = g_board->pieces[to];

  g_moves[g_moves_n]     = *g_board;
  g_board                = &g_moves[g_moves_n];
  g_board->from          = from;
  g_board->to            = to;
  g_board->score         = 0;
  g_board->type          = 0;
  g_board->pieces[from]  = 0;
  g_board->pieces[to]    = me;
  g_board->white[me - 1] = (g_board->white[me - 1] ^ Bit(from)) | Bit(to);
  ++g_board->fifty; // Rule50 counter increased after non-decisive move

  CheckNormalCapturesW(me, eat, to);
  ModifyPawnStuffW(to);
  AddMoveIfOkW();
  g_board = g_board_orig; // Back to old board
}

void AddNormalStuffB(const int from, const int to) {
  const auto me = g_board->pieces[from], eat = g_board->pieces[to];

  g_moves[g_moves_n]      = *g_board;
  g_board                 = &g_moves[g_moves_n];
  g_board->from           = from;
  g_board->to             = to;
  g_board->score          = 0;
  g_board->type           = 0;
  g_board->pieces[to]     = me;
  g_board->pieces[from]   = 0;
  g_board->black[-me - 1] = (g_board->black[-me - 1] ^ Bit(from)) | Bit(to);
  ++g_board->fifty;

  CheckNormalCapturesB(me, eat, to);
  ModifyPawnStuffB(to);
  AddMoveIfOkB();
  g_board = g_board_orig;
}

void AddW(const int from, const int to) {
  g_board->pieces[from] == +1 && Yaxl(from) == 6 ? AddPromotionStuffW(from, to) : AddNormalStuffW(from, to);
}

void AddB(const int from, const int to) {
  g_board->pieces[from] == -1 && Yaxl(from) == 1 ? AddPromotionStuffB(from, to) : AddNormalStuffB(from, to);
}

void AddMovesW(const int from, std::uint64_t moves) {
  while (moves) AddW(from, CtzPop(&moves));
}

void AddMovesB(const int from, std::uint64_t moves) {
  while (moves) AddB(from, CtzPop(&moves));
}

void MgenPawnsW() {
  for (auto p = g_board->white[0]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesW(sq, (g_pawn_checks_w[sq] & g_pawn_sq) | (g_pawn_1_moves_w[sq] & g_empty));
  }
}

void MgenPawnsB() {
  for (auto p = g_board->black[0]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesB(sq, ((g_pawn_checks_b[sq] & g_pawn_sq) | (g_pawn_1_moves_b[sq] & g_empty)));
  }
}

void MgenPawnsOnlyCapturesW() {
  for (auto p = g_board->white[0]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesW(sq, g_pawn_checks_w[sq] & g_pawn_sq);
  }
}

void MgenPawnsOnlyCapturesB() {
  for (auto p = g_board->black[0]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesB(sq, g_pawn_checks_b[sq] & g_pawn_sq);
  }
}

void MgenKnightsW() {
  for (auto p = g_board->white[1]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesW(sq, g_knight_moves[sq] & g_good);
  }
}

void MgenKnightsB() {
  for (auto p = g_board->black[1]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesB(sq, g_knight_moves[sq] & g_good);
  }
}

void MgenBishopsW() {
  for (auto p = g_board->white[2]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesW(sq, g_bishop_moves[sq] & g_good);
  }
}

void MgenBishopsB() {
  for (auto p = g_board->black[2]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesB(sq, g_bishop_moves[sq] & g_good);
  }
}

void MgenRooksW() {
  for (auto p = g_board->white[3]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesW(sq, g_rook_moves[sq] & g_good);
  }
}

void MgenRooksB() {
  for (auto p = g_board->black[3]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesB(sq, g_rook_moves[sq] & g_good);
  }
}

void MgenQueensW() {
  for (auto p = g_board->white[4]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesW(sq, g_queen_moves[sq] & g_good);
  }
}

void MgenQueensB() {
  for (auto p = g_board->black[4]; p; ) {
    const auto sq = CtzPop(&p);
    AddMovesB(sq, g_queen_moves[sq] & g_good);
  }
}

void MgenKingW() {
  const auto sq = std::countr_zero(g_board->white[5]);
  AddMovesW(sq, g_king_moves[sq] & g_good);
}

void MgenKingB() {
  const auto sq = std::countr_zero(g_board->black[5]);
  AddMovesB(sq, g_king_moves[sq] & g_good);
}

inline void MgenSetupBoth() {
  g_white = White();
  g_black = Black();
  g_both  = g_white | g_black;
  g_empty = ~g_both;
}

void MgenSetupW() {
  MgenSetupBoth();
  g_pawn_sq = g_black;
}

void MgenSetupB() {
  MgenSetupBoth();
  g_pawn_sq = g_white;
}

void MgenAllW() {
  MgenSetupW();
  g_good = ~g_white;
  MgenPawnsW();
  MgenKnightsW();
  MgenBishopsW();
  MgenRooksW();
  MgenQueensW();
  MgenKingW();
}

void MgenAllB() {
  MgenSetupB();
  g_good = ~g_black;
  MgenPawnsB();
  MgenKnightsB();
  MgenBishopsB();
  MgenRooksB();
  MgenQueensB();
  MgenKingB();
}

void MgenAllCapturesW() {
  MgenSetupW();
  g_good = g_black;
  MgenPawnsOnlyCapturesW();
  MgenKnightsW();
  MgenBishopsW();
  MgenRooksW();
  MgenQueensW();
  MgenKingW();
}

void MgenAllCapturesB() {
  MgenSetupB();
  g_good = g_white;
  MgenPawnsOnlyCapturesB();
  MgenKnightsB();
  MgenBishopsB();
  MgenRooksB();
  MgenQueensB();
  MgenKingB();
}

inline void MgenReset(Board *moves) {
  g_moves_n    = 0;
  g_moves      = moves;
  g_board_orig = g_board;
}

// Generate everything
int MgenW(Board *moves) {
  MgenReset(moves);
  MgenAllW();
  return g_moves_n;
}

int MgenB(Board *moves) {
  MgenReset(moves);
  MgenAllB();
  return g_moves_n;
}

// Generate only captures
int MgenCapturesW(Board *moves) {
  MgenReset(moves);
  MgenAllCapturesW();
  return g_moves_n;
}

int MgenCapturesB(Board *moves) {
  MgenReset(moves);
  MgenAllCapturesB();
  return g_moves_n;
}

// All moves if under checks or just captures
int MgenTacticalW(Board *moves) {
  return ChecksB() ? MgenW(moves) : MgenCapturesW(moves);
}

int MgenTacticalB(Board *moves) {
  return ChecksW() ? MgenB(moves) : MgenCapturesB(moves);
}

// Generate only root moves
void MgenRoot() {
  g_root_n = g_wtm ? MgenW(g_boards[0]) : MgenB(g_boards[0]);
}

// Evaluation

// Detect trivial draws really fast
bool EasyDraw() {
  if (g_board->white[3] | g_board->white[4] | g_board->black[3] | g_board->black[4]) return false;

  const auto pawns = g_board->white[0] | g_board->black[0];
  if (const auto nnbb = g_board->white[1] | g_board->white[2] | g_board->black[1] | g_board->black[2]; nnbb)
    return pawns ? false : std::popcount(nnbb) <= 1;

  return std::popcount(pawns) == 0;
}

int FixFRC() {
  // No bishop in corner -> Speedup
  constexpr std::uint64_t corners = Bit(0) | Bit(7) | Bit(56) | Bit(63);
  if (!((g_board->white[2] | g_board->black[2]) & corners)) return 0;

  auto s = 0;
  if (g_board->pieces[0]  == +3 && g_board->pieces[9]  == +1) s -= kFrcPenalty;
  if (g_board->pieces[7]  == +3 && g_board->pieces[14] == +1) s -= kFrcPenalty;
  if (g_board->pieces[56] == -3 && g_board->pieces[49] == -1) s += kFrcPenalty;
  if (g_board->pieces[63] == -3 && g_board->pieces[54] == -1) s += kFrcPenalty;
  return s;
}

// HCE

inline int FlipY(const int sq) { return sq ^ 56; } // Mirror horizontal
inline int Square(const int x) { return x * x; } // x ^ 2
int CloseBonus(const int a, const int b) { return Square(7 - std::abs(Xaxl(a) - Xaxl(b))) + Square(7 - std::abs(Yaxl(a) - Yaxl(b))); }
int CloseAnyCornerBonus(const int sq) { return std::max({CloseBonus(sq, 0), CloseBonus(sq, 7), CloseBonus(sq, 56), CloseBonus(sq, 63)}); }

struct Evaluation {
  const std::uint64_t white{0}, black{0}, both{0};
  const bool wtm{true};
  int w_pieces[5]{}, b_pieces[5]{}, white_total{1}, black_total{1}, both_total{0}, piece_sum{0},
      wk{0}, bk{0}, score{0}, mg{0}, eg{0}, scale_factor{1};

  Evaluation* pesto_w(const int p, const int sq) {
    this->mg += kPestoPsqt[p][0][kHalfSquare[sq]] + kPestoMaterial[0][p];
    this->eg += kPestoPsqt[p][1][kHalfSquare[sq]] + kPestoMaterial[1][p];
    return this;
  }

  Evaluation* pesto_b(const int p, const int sq) {
    this->mg -= kPestoPsqt[p][0][kHalfSquare[FlipY(sq)]] + kPestoMaterial[0][p];
    this->eg -= kPestoPsqt[p][1][kHalfSquare[FlipY(sq)]] + kPestoMaterial[1][p];
    return this;
  }

  std::uint64_t reachable_w() const { return ~this->white; } // Squares not having own pieces are reachable
  std::uint64_t reachable_b() const { return ~this->black; }

  Evaluation* mobility_w(const int k, const std::uint64_t m) { this->score += k * std::popcount(m); return this; }
  Evaluation* mobility_b(const int k, const std::uint64_t m) { this->score -= k * std::popcount(m); return this; }

  inline Evaluation* eval_score_w(const int piece, const int k, const int sq, const std::uint64_t m) {
    return this->pesto_w(piece, sq)->mobility_w(k, m & this->reachable_w());
  }

  inline Evaluation* eval_score_b(const int piece, const int k, const int sq, const std::uint64_t m) {
    return this->pesto_b(piece, sq)->mobility_b(k, m & this->reachable_b());
  }

  inline void eval_w(const int p) { this->piece_sum += kPiece[p]; ++this->white_total; ++this->w_pieces[p]; }
  inline void eval_b(const int p) { this->piece_sum += kPiece[p]; ++this->black_total; ++this->b_pieces[p]; }

  void pawn_w(const int sq)   { this->pesto_w(0, sq)->eval_w(0); }
  void pawn_b(const int sq)   { this->pesto_b(0, sq)->eval_b(0); }
  void knight_w(const int sq) { this->eval_score_w(1, 2, sq, g_knight_moves[sq])->eval_w(1); }
  void knight_b(const int sq) { this->eval_score_b(1, 2, sq, g_knight_moves[sq])->eval_b(1); }
  void bishop_w(const int sq) { this->eval_score_w(2, 3, sq, g_bishop_moves[sq])->eval_w(2); }
  void bishop_b(const int sq) { this->eval_score_b(2, 3, sq, g_bishop_moves[sq])->eval_b(2); }
  void rook_w(const int sq)   { this->eval_score_w(3, 3, sq, g_rook_moves[sq])->eval_w(3); }
  void rook_b(const int sq)   { this->eval_score_b(3, 3, sq, g_rook_moves[sq])->eval_b(3); }
  void queen_w(const int sq)  { this->eval_score_w(4, 2, sq, g_queen_moves[sq])->eval_w(4); }
  void queen_b(const int sq)  { this->eval_score_b(4, 2, sq, g_queen_moves[sq])->eval_b(4); }
  void king_w(const int sq)   { this->eval_score_w(5, 1, sq, g_king_moves[sq])->wk = sq; }
  void king_b(const int sq)   { this->eval_score_b(5, 1, sq, g_king_moves[sq])->bk = sq; }

  void eval_piece(const int sq) {
    switch (g_board->pieces[sq]) {
      case +1: this->pawn_w(sq);   break;
      case +2: this->knight_w(sq); break;
      case +3: this->bishop_w(sq); break;
      case +4: this->rook_w(sq);   break;
      case +5: this->queen_w(sq);  break;
      case +6: this->king_w(sq);   break; // White king (+1) is already in
      case -1: this->pawn_b(sq);   break;
      case -2: this->knight_b(sq); break;
      case -3: this->bishop_b(sq); break;
      case -4: this->rook_b(sq);   break;
      case -5: this->queen_b(sq);  break;
      case -6: this->king_b(sq);   break; // Black king (+1) is already in
    }
  }

  Evaluation* evaluate_pieces() {
    for (auto b = this->both; b; ) this->eval_piece(CtzPop(&b));
    this->both_total = this->white_total + this->black_total;
    return this;
  }

  Evaluation* bonus_tempo() {
    this->score += this->wtm ? +kTempoBonus : -kTempoBonus;
    return this;
  }

  Evaluation* bonus_checks() {
    if (     ChecksW()) this->score += kChecksBonus;
    else if (ChecksB()) this->score -= kChecksBonus;
    return this;
  }

  // Force enemy king in the corner and get closer
  void bonus_mating_w() { this->score += 6 * CloseAnyCornerBonus(this->bk) + 4 * CloseBonus(this->wk, this->bk); }
  void bonus_mating_b() { this->score -= 6 * CloseAnyCornerBonus(this->wk) + 4 * CloseBonus(this->bk, this->wk); }

  Evaluation* bonus_endgame() {
    if (     this->black_total == 1) this->bonus_mating_w();
    else if (this->white_total == 1) this->bonus_mating_b();
    return this;
  }

  int calculate_score() const { // 78 phases for HCE
    const float n = float(std::clamp(this->piece_sum, 0, kMaxPieces)) / float(kMaxPieces);
    const int s   = int(n * float(this->mg) + (1.0f - n) * float(this->eg));
    return (this->score + s) / this->scale_factor;
  }

  int evaluate() {
    return this->evaluate_pieces()
               ->bonus_tempo()
               ->bonus_checks()
               ->bonus_endgame()
               ->calculate_score();
  }
};

// NNUE Eval

int EvaluateClassical(const bool wtm) {
  return Evaluation { .white = White(), .black = Black(), .both = Both(), .wtm = wtm } .evaluate();
}

// Add noise to eval for different playing levels ( -5 -> +5 pawns )
// 0    (Random Mover)
// 1-99 (Levels)
// 100  (Full Strength)
int LevelNoise() { return Random(-5 * (100 - g_level), +5 * (100 - g_level)); }

// Shuffle period 30 plies then scale
float GetScale() { return g_board->fifty < 30 ? 1.0f : (1.0f - ((float(g_board->fifty - 30)) / 110.0f)); }
float GetEval(const bool wtm) { return FixFRC() + EvaluateClassical(wtm); }
int Evaluate(const bool wtm) { return LevelNoise() + (EasyDraw() ? 0 : (GetScale() * GetEval(wtm))); }

// Search

void SpeakUci(const int score, const std::uint64_t ms) {
  std::cout << "info depth " << std::min(g_max_depth, g_depth + 1) <<
    " nodes " << g_nodes <<
    " time " << ms <<
    " nps " << Nps(g_nodes, ms) <<
    " score cp " << ((g_wtm ? +1 : -1) * (std::abs(score) == kInf ? score / 100 : score)) <<
    " pv " << g_boards[0][0].movename() << std::endl; // flush
}

bool Draw() {
  if (g_board->fifty > kFifty || EasyDraw()) return true; // Checkmate overrules rule50 ( == 100 )

  const auto hash = g_r50_positions[g_board->fifty]; // g_r50_positions.pop() must contain hash !
  for (int i = g_board->fifty - 2, reps = 0; i >= 0; i -= 2)
    if (g_r50_positions[i] == hash && ++reps == kRepsDraw - 1)
      return true;

  return false;
}

// Responding to "quit" / "stop" / "isready" signals
bool UserStop() {
  if (!InputAvailable()) return false;

  ReadInput();
  if (Token("isready")) {
    std::cout << "readyok" << std::endl;
    return false;
  }

  return Token("quit") ? !(g_game_on = false) : Token("stop");
}

inline bool CheckTime() { // Time checking
  static std::uint64_t ticks = 0;
  return ((++ticks) & kReadClockTicks) ? false : ((Now() >= g_stop_search_time) || UserStop());
}

// 1. Check against standpat to see whether we are better -> Done
// 2. Iterate deeper
int QSearchW(int alpha, const int beta, const int depth, const int ply) {
  ++g_nodes; // Increase visited nodes count

  if (g_stop_search || (g_stop_search = CheckTime())) return 0; // Search is stopped. Return ASAP
  if (((alpha = std::max(alpha, Evaluate(true))) >= beta) || depth <= 0) return alpha; // Better / terminal node -> Done

  const auto moves_n = MgenTacticalW(g_boards[ply]);
  for (auto i = 0; i < moves_n; ++i) {
    LazySort(ply, i, moves_n); // Very few moves, sort them all
    g_board = g_boards[ply] + i;
    if ((alpha = std::max(alpha, QSearchB(alpha, beta, depth - 1, ply + 1))) >= beta) return alpha;
  }

  return alpha;
}

int QSearchB(const int alpha, int beta, const int depth, const int ply) {
  ++g_nodes;

  if (g_stop_search) return 0;
  if ((alpha >= (beta = std::min(beta, Evaluate(false)))) || depth <= 0) return beta;

  const auto moves_n = MgenTacticalB(g_boards[ply]);
  for (auto i = 0; i < moves_n; ++i) {
    LazySort(ply, i, moves_n);
    g_board = g_boards[ply] + i;
    if (alpha >= (beta = std::min(beta, QSearchW(alpha, beta, depth - 1, ply + 1)))) return beta;
  }

  return beta;
}

void SetPv(const int ply, const int move_i) { g_board = g_boards[ply] + move_i, g_is_pv = move_i <= 1 && !g_board->score; }
int GetLmr(const int d, const int m) { return (d <= 0 || m <= 0) ? 1 : std::clamp<int>(0.25 * std::log(d) * std::log(m), 1, 6); }

// a >= b -> Minimizer won't pick any better move anyway.
//           So searching beyond is a waste of time.
int SearchMovesW(int alpha, const int beta, int depth, const int ply) {
  const auto hash    = g_r50_positions[g_board->fifty];
  const auto checks  = ChecksB();
  const auto moves_n = MgenW(g_boards[ply]);

  if (!moves_n) return checks ? -kInf : 0; // Checkmate or stalemate
  if (moves_n == 1 || (depth == 1 && (checks || g_board->type == 8))) ++depth; // Extend interesting path (SRE / CE / PPE)

  const auto ok_lmr = moves_n >= 5 && depth >= 2 && !checks;
  auto *entry       = &g_hash[std::uint32_t(hash % g_hash_entries)];
  entry->put_hash_value_to_moves(hash, g_boards[ply]);

  // Tiny speedup since not all moves are scored (lots of pointless shuffling ...)
  // So avoid sorting useless moves
  auto sort = true;
  for (auto i = 0; i < moves_n; ++i) {
    if (sort) LazySort(ply, i, moves_n), sort = g_boards[ply][i].score != 0;
    SetPv(ply, i);
    if (ok_lmr && i >= 1 && !g_board->score && !ChecksW()) {
      if (SearchB(alpha, beta, depth - 2 - GetLmr(depth, i), ply + 1) <= alpha) continue;
      g_board = g_boards[ply] + i;
    }
    if (const auto score = SearchB(alpha, beta, depth - 1, ply + 1); score > alpha) { // Improved scope
      if ((alpha = score) >= beta) {
        entry->update<MoveType::kKiller>(hash, g_boards[ply][i].index);
        return alpha;
      }
      entry->update<MoveType::kGood>(hash, g_boards[ply][i].index);
    }
  }

  return alpha;
}

int SearchMovesB(const int alpha, int beta, int depth, const int ply) {
  const auto hash    = g_r50_positions[g_board->fifty];
  const auto checks  = ChecksW();
  const auto moves_n = MgenB(g_boards[ply]);

  if (!moves_n) return checks ? +kInf : 0;
  if (moves_n == 1 || (depth == 1 && (checks || g_board->type == 8))) ++depth;

  const auto ok_lmr = moves_n >= 5 && depth >= 2 && !checks;
  auto *entry       = &g_hash[std::uint32_t(hash % g_hash_entries)];
  entry->put_hash_value_to_moves(hash, g_boards[ply]);

  auto sort = true;
  for (auto i = 0; i < moves_n; ++i) {
    if (sort) LazySort(ply, i, moves_n), sort = g_boards[ply][i].score != 0;
    SetPv(ply, i);
    if (ok_lmr && i >= 1 && !g_board->score && !ChecksB()) {
      if (SearchW(alpha, beta, depth - 2 - GetLmr(depth, i), ply + 1) >= beta) continue;
      g_board = g_boards[ply] + i;
    }
    if (const auto score = SearchW(alpha, beta, depth - 1, ply + 1); score < beta) {
      if (alpha >= (beta = score)) {
        entry->update<MoveType::kKiller>(hash, g_boards[ply][i].index);
        return beta;
      }
      entry->update<MoveType::kGood>(hash, g_boards[ply][i].index);
    }
  }

  return beta;
}

// If we do nothing and we are still better -> Done
bool TryNullMoveW(int *alpha, const int beta, const int depth, const int ply) {
  if ((!g_nullmove_active) && // No nullmove on the path ?
      (!g_is_pv) && // Not pv ?
      (depth >= 3) && // Enough depth ( 2 blunders too much. 3 sweet spot ... ) ?
      ((g_board->white[1] | g_board->white[2] | g_board->white[3] | g_board->white[4]) ||
       (std::popcount(g_board->white[0]) >= 2)) && // Non pawn material or at least 2 pawns ( Zugzwang ... ) ?
      (!ChecksB()) && // Not under checks ?
      (Evaluate(true) >= beta)) { // Looks good ?
    auto *tmp         = g_board;
    g_nullmove_active = true;
    const auto score  = SearchB(*alpha, beta, depth - int(depth / 4 + 3), ply);
    g_nullmove_active = false;
    g_board           = tmp;
    if (score >= beta) {
      *alpha = score;
      return true;
    }
  }
  return false;
}

bool TryNullMoveB(const int alpha, int *beta, const int depth, const int ply) {
  if ((!g_nullmove_active) &&
      (!g_is_pv) &&
      (depth >= 3) &&
      ((g_board->black[1] | g_board->black[2] | g_board->black[3] | g_board->black[4]) ||
       (std::popcount(g_board->black[0]) >= 2)) &&
      (!ChecksW()) &&
      (alpha >= Evaluate(false))) {
    auto *tmp         = g_board;
    g_nullmove_active = true;
    const auto score  = SearchW(alpha, *beta, depth - int(depth / 4 + 3), ply);
    g_nullmove_active = false;
    g_board           = tmp;
    if (alpha >= score) {
      *beta = score;
      return true;
    }
  }
  return false;
}

// Front-end for ab-search
int SearchW(int alpha, const int beta, const int depth, const int ply) {
  ++g_nodes;

  if (g_stop_search || (g_stop_search = CheckTime())) return 0; // Search is stopped. Return ASAP
  if (depth <= 0 || ply >= kMaxDepth) return QSearchW(alpha, beta, g_q_depth, ply);

  const auto fifty = g_board->fifty;
  const auto tmp   = g_r50_positions[fifty];

  if (TryNullMoveW(&alpha, beta, depth, ply)) return alpha;

  g_r50_positions[fifty] = Hash(true);
  alpha                  = Draw() ? 0 : SearchMovesW(alpha, beta, depth, ply);
  g_r50_positions[fifty] = tmp;

  return alpha;
}

int SearchB(const int alpha, int beta, const int depth, const int ply) {
  ++g_nodes;

  if (g_stop_search) return 0;
  if (depth <= 0 || ply >= kMaxDepth) return QSearchB(alpha, beta, g_q_depth, ply);

  const auto fifty = g_board->fifty;
  const auto tmp   = g_r50_positions[fifty];

  if (TryNullMoveB(alpha, &beta, depth, ply)) return beta;

  g_r50_positions[fifty] = Hash(false);
  beta                   = Draw() ? 0 : SearchMovesB(alpha, beta, depth, ply);
  g_r50_positions[fifty] = tmp;

  return beta;
}

int FindBestW(const int i, const int alpha) {
  if (g_depth >= 1 && i >= 1) { // Null window search for bad moves
    if (const int score = SearchB(alpha, alpha + 1, g_depth, 1); score > alpha) {
      SetPv(0, i);
      return SearchB(alpha, +kInf, g_depth, 1); // Search w/ full window
    } else {
      return score;
    }
  }
  return SearchB(alpha, +kInf, g_depth, 1);
}

// Root search
int BestW() {
  auto best_i = 0, alpha = -kInf;

  for (auto i = 0; i < g_root_n; ++i) {
    SetPv(0, i); // 1 / 2 moves too good and not tactical -> pv
    const auto score = FindBestW(i, alpha);
    if (g_stop_search) return g_best_score; // Scores are rubbish now
    if (score > alpha) {
      alpha  = score;
      best_i = i;
    }
  }

  SortRoot(best_i);
  return alpha;
}

int FindBestB(const int i, const int beta) {
  if (g_depth >= 1 && i >= 1) {
    if (const int score = SearchW(beta - 1, beta, g_depth, 1); score < beta) {
      SetPv(0, i);
      return SearchW(-kInf, beta, g_depth, 1);
    } else {
      return score;
    }
  }
  return SearchW(-kInf, beta, g_depth, 1);
}

int BestB() {
  auto best_i = 0, beta = +kInf;

  for (auto i = 0; i < g_root_n; ++i) {
    SetPv(0, i);
    const auto score = FindBestB(i, beta);
    if (g_stop_search) return g_best_score;
    if (score < beta) {
      beta   = score;
      best_i = i;
    }
  }

  SortRoot(best_i);
  return beta;
}

bool RandomMove() { // At level 0 we simply play a random move
  if (g_level == 0) {
    SwapMoveInRootList(Random(0, g_root_n - 1));
    return true;
  }
  return false;
}

bool FastMove(const int ms) {
  if ((g_root_n <= 1) || // Only move
      (RandomMove())  || // Random mover
      (ms <= 1)) {       // Hurry up !
    SpeakUci(g_last_eval, 0);
    return true;
  }
  return false;
}

void SearchRootMoves() {
  const auto start = Now();

  for ( ; std::abs(g_best_score) != kInf && g_depth < g_max_depth && !g_stop_search; ++g_depth) {
    g_q_depth    = std::min(g_q_depth + 2, kMaxQDepth);
    g_best_score = g_wtm ? BestW() : BestB();
    SpeakUci(g_best_score, Now() - start);
  }

  g_last_eval = g_best_score;
  if (!g_q_depth) SpeakUci(g_last_eval, Now() - start); // Nothing searched -> Print smt for UCI
}

void ThinkReset() { // Reset search status
  g_stop_search = g_nullmove_active = g_is_pv = false;
  g_q_depth = g_best_score = g_nodes = g_depth = 0;
}

void Think(const int ms) {
  g_stop_search_time = Now() + std::uint64_t(ms); // Start clock early
  ThinkReset();
  MgenRoot();
  if (!g_analyzing && FastMove(ms)) return;

  const auto tmp = g_board;
  EvalRootMoves();
  SortRootMoves();

  SearchRootMoves();
  g_board = tmp; // Just in case ...
}

// Perft

std::uint64_t Perft(const bool wtm, const int depth, const int ply) {
  if (depth <= 0) return 1;
  const auto moves_n = wtm ? MgenW(g_boards[ply]) : MgenB(g_boards[ply]);
  if (depth == 1) return moves_n; // Bulk counting
  std::uint64_t nodes = 0;
  for (auto i = 0; i < moves_n; ++i) g_board = g_boards[ply] + i, nodes += Perft(!wtm, depth - 1, ply + 1);
  return nodes;
}

// UCI

void UciMake(const int root_i) {
  if (!g_wtm) ++g_fullmoves; // Increase fullmoves only after black move
  g_r50_positions[std::min<std::size_t>(g_board->fifty, kFifty)] = Hash(g_wtm); // Set hash
  g_board_empty = g_boards[0][root_i]; // Copy current board
  g_board       = &g_board_empty; // Set pointer ( g_board must always point to smt )
  g_wtm         = !g_wtm; // Flip the board
}

void UciMakeMove() {
  const auto move = TokenNth();
  MgenRoot();
  for (auto i = 0; i < g_root_n; ++i)
    if (move == g_boards[0][i].movename()) {
      UciMake(i);
      return;
    }
  throw std::runtime_error("info string Bad move: " + move); // No move found -> Quit
}

void UciTakeSpecialFen() {
  TokenPop(); // pop "fen"
  std::stringstream fen{};
  for ( ; TokenOk() && !TokenPeek("moves"); TokenPop())
    fen << TokenNth() << " ";
  SetFen(fen.str());
}

void UciFen() {
  Token("startpos") ? SetFen(kStartPos) : UciTakeSpecialFen();
}

void UciMoves() {
  for ( ; TokenOk(); TokenPop()) UciMakeMove();
}

void UciPosition() {
  UciFen();
  if (Token("moves")) UciMoves();
}

void UciSetoption() {
  if (TokenPeek("name") && TokenPeek("value", 2)) {
    if (TokenPeek("Hash", 1))              { SetHashtable(TokenNumber(3)); }
    else if (TokenPeek("Level", 1))        { g_level = std::clamp(TokenNumber(3), 0, 100); }
    else if (TokenPeek("MoveOverhead", 1)) { g_move_overhead = std::clamp(TokenNumber(3), 0, 10000); }
  }
}

void PrintBestMove() {
  std::cout << "bestmove " << (g_root_n <= 0 ? "0000" : g_boards[0][0].movename()) << std::endl;
}

void UciGoInfinite() {
  g_analyzing = true;
  Think(kWeek);
  g_analyzing = false;
  PrintBestMove();
}

void UciGoMovetime() {
  Think(std::max(0, TokenNumber()));
  PrintBestMove();
}

void UciGoDepth() {
  g_max_depth = std::clamp(TokenNumber(), 1, kMaxDepth);
  Think(kWeek);
  g_max_depth = kMaxDepth;
  PrintBestMove();
}

// Calculate needed time then think
// Make sure we never lose on time
// Thus small overheadbuffer (100 ms) to prevent time losses
void UciGo() {
  auto wtime = 0, btime = 0, winc = 0, binc = 0, mtg = 26;

  for ( ; TokenOk(); TokenPop())
    if (     Token("wtime"))     { wtime = std::max(0, TokenNumber() - g_move_overhead); }
    else if (Token("btime"))     { btime = std::max(0, TokenNumber() - g_move_overhead); }
    else if (Token("winc"))      { winc  = std::max(0, TokenNumber()); }
    else if (Token("binc"))      { binc  = std::max(0, TokenNumber()); }
    else if (Token("movestogo")) { mtg   = std::max(1, TokenNumber()); }
    else if (Token("movetime"))  { UciGoMovetime(), TokenPop(); return; }
    else if (Token("infinite"))  { UciGoInfinite();             return; }
    else if (Token("depth"))     { UciGoDepth(), TokenPop();    return; }

  g_wtm ? Think(std::min(wtime, wtime / mtg + winc)) :
          Think(std::min(btime, btime / mtg + binc));

  PrintBestMove();
}

void UciUci() {
  std::cout <<
    "id name " << kVersion << '\n' <<
    "id author Toni Helminen\n" <<
    "option name Level type spin default 100 min 0 max 100\n" <<
    "option name MoveOverhead type spin default " << kMoveOverhead << " min 0 max 10000\n" <<
    "option name Hash type spin default " << kHashMB << " min 1 max 1048576\n" <<
    "uciok" << std::endl;
}

// Save state (just in case) if multiple commands in a row
struct Save {
  const std::string fen{};
  Save() : fen{g_board->to_fen()} { }
  ~Save() { SetFen(this->fen); }
};

// Print ASCII art board ( + Used for debug in UCI mode )
void UciPrintBoard(const std::string &fen) {
  const Save save{};
  if (fen.length()) SetFen(fen);
  std::cout << '\n' << g_board->to_s() << std::endl;
}

// Calculate perft split numbers
void UciPerft(const std::string &d, const std::string &fen) {
  const Save save{};
  const auto depth = d.length() ? std::max(0, std::stoi(d)) : 7;
  std::uint64_t nodes = depth >= 1 ? 0 : 1, total_ms = 0;
  SetFen(fen.length() ? fen : kStartPos);
  MgenRoot();
  for (auto i = 0; i < g_root_n; ++i) {
    g_board           = g_boards[0] + i;
    const auto start  = Now();
    const auto nodes2 = depth >= 0 ? Perft(!g_wtm, depth - 1, 1) : 0;
    const auto ms     = Now() - start;
    std::cout << (i + 1) << ". " << g_boards[0][i].movename() << " -> " << nodes2 << " (" << ms << " ms)" << std::endl;
    nodes    += nodes2;
    total_ms += ms;
  }
  std::cout << "\n===========================\n\n" <<
    "Nodes:    " << nodes << '\n' <<
    "Time(ms): " << total_ms << '\n' <<
    "NPS:      " << Nps(nodes, total_ms) << std::endl;
}

// Bench signature and speed of the program
void UciBench(const std::string &d, const std::string &t, const std::string &h) {
  const Save save{};
  SetHashtable(h.length() ? std::stoi(h) : 256); // Set hash and reset
  g_max_depth = !d.length() ? 17 : (d == "inf" ? kMaxDepth : std::clamp(std::stoi(d), 0, kMaxDepth)); // Set depth limits
  g_noise     = 0; // Make search deterministic
  std::uint64_t nodes = 0;
  const auto time     = !t.length() || t == "inf" ? kInf : std::max(0, std::stoi(t)); // Set time limits
  auto n = 0, total_ms = 0, correct = 0;
  for (const auto &fen : kBench) {
    std::cout << "[ " << (++n) << "/" << kBench.size() << " ; "  << fen << " ]" << std::endl;
    SetFen(fen);
    const auto start = Now();
    Think(time);
    total_ms += Now() - start;
    nodes    += g_nodes;
    std::cout << std::endl;
    if (g_boards[0][0].movename() == fen.substr(fen.rfind(" bm ") + 4)) ++correct;
  }
  g_noise     = kNoise;
  g_max_depth = kMaxDepth;
  std::cout << "===========================\n\n" <<
    "Result:   " << correct << " / " << kBench.size() << '\n' <<
    "Nodes:    " << nodes << '\n' <<
    "Time(ms): " << total_ms << '\n' <<
    "NPS:      " << Nps(nodes, total_ms) << std::endl;
}

void UciRules() {
  std::cout <<
    "The rules are optimized for simplicity.\n" <<
    "To have very simple engine code.\n\n" <<
    "- Normal chess rules apply except ...\n" <<
    "- 2 reps is a draw\n" <<
    "- Pawns move normally but only 1 move\n" <<
    "- B/R move normally but only 1 move\n" <<
    "- N moves normally\n" <<
    "- Q = K + N\n" <<
    "- K moves normally\n" <<
    "- No underpromotion. Pawns only promote to Q\n" <<
    "- No castling\n" <<
    "- No en passant\n" <<
    "- No 2 step pawn push" << std::endl;
}

void UciHelp() {
  std::cout <<
    "Cain. Linux UCI SmallChess engine. Written in C++20 language\n\n" <<
    "help        This help\n" <<
    "uci         Outputs the engine info\n" <<
    "isready     Synchronization of the engine. Responded w/ 'readyok'\n" <<
    "ucinewgame  Sent before the game\n" <<
    "stop        Stop the search and report a bestmove\n" <<
    "quit        Exits the engine ASAP\n" <<
    "rules       Print SmallChess rules\n" <<
    "setoption name [str] value [str]\n" <<
    "            Sets a given option ( See 'uci' )\n" <<
    "go wtime [int] btime [int] winc [int] binc [int]\n" <<
    "            ... movestogo [int] movetime [int] depth [int] [infinite]\n" <<
    "            Search the current position with the provided settings\n" <<
    "p [fen]     Print ASCII art board\n" <<
    "position [startpos | fen] [moves]?\n" <<
    "            Sets the board position via an optional FEN and optional move list\n" <<
    "perft [depth] [fen]\n" <<
    "            Calculate perft split numbers\n" <<
    "            > perft ( 378087602 )\n" <<
    "bench [depth] [time] [hash]\n"  <<
    "            Bench signature and speed of the program\n" <<
    "            > bench ( 182396163 )" << std::endl;
}

bool UciCommands() {
  if (!TokenOk()) return true;

  if (     Token("position"))   UciPosition();
  else if (Token("go"))         UciGo();
  else if (Token("ucinewgame")) g_last_eval = 0;
  else if (Token("isready"))    std::cout << "readyok" << std::endl;
  else if (Token("setoption"))  UciSetoption();
  else if (Token("uci"))        UciUci();
  else if (Token("quit"))       return false;
  // Extra ...
  else if (Token("help"))       UciHelp();
  else if (Token("rules"))      UciRules();
  else if (Token("bench"))      UciBench(TokenNth(0), TokenNth(1), TokenNth(2));
  else if (Token("perft"))      UciPerft(TokenNth(0), TokenNth(1));
  else if (Token("p"))          UciPrintBoard(TokenNth(0));
  else                          std::cout << "Unknown command: " << TokenNth(0) << std::endl;

  return g_game_on;
}

bool Uci() {
  ReadInput();
  return UciCommands();
}

// Init

std::uint64_t MakeJumpMoves(const int sq, const std::vector<int> &jump_vectors) {
  std::uint64_t moves = 0;
  const auto x_pos = Xaxl(sq), y_pos = Yaxl(sq);
  for (std::size_t i = 0; i < jump_vectors.size() / 2; ++i)
    if (const auto x = x_pos + jump_vectors[2 * i], y = y_pos + jump_vectors[2 * i + 1]; OnBoard(x, y))
      moves |= Bit(8 * y + x);
  return moves;
}

void InitJumpMoves() {
  const std::vector<int> king_vectors         = {+1,  0,  0, +1,  0, -1, -1,  0, +1, +1, -1, -1, +1, -1, -1, +1};
  const std::vector<int> knight_vectors       = {+2, +1, -2, +1, +2, -1, -2, -1, +1, +2, -1, +2, +1, -2, -1, -2};
  const std::vector<int> pawn_check_vectors_w = {-1, +1, +1, +1};
  const std::vector<int> pawn_check_vectors_b = {-1, -1, +1, -1};
  const std::vector<int> bishop_vectors       = {+1, +1, -1, -1, +1, -1, -1, +1};
  const std::vector<int> rook_vectors         = {+1,  0,  0, +1,  0, -1, -1,  0};
  const std::vector<int> pawn_1_vectors_w     = { 0, +1};
  const std::vector<int> pawn_1_vectors_b     = { 0, -1};

  for (auto i = 0; i < 64; ++i) {
    g_queen_moves[i]    = MakeJumpMoves(i, king_vectors) | MakeJumpMoves(i, knight_vectors);
    g_bishop_moves[i]   = MakeJumpMoves(i, bishop_vectors);
    g_rook_moves[i]     = MakeJumpMoves(i, rook_vectors);
    g_king_moves[i]     = MakeJumpMoves(i, king_vectors);
    g_knight_moves[i]   = MakeJumpMoves(i, knight_vectors);
    g_pawn_checks_w[i]  = MakeJumpMoves(i, pawn_check_vectors_w);
    g_pawn_checks_b[i]  = MakeJumpMoves(i, pawn_check_vectors_b);
    g_pawn_1_moves_w[i] = MakeJumpMoves(i, pawn_1_vectors_w);
    g_pawn_1_moves_b[i] = MakeJumpMoves(i, pawn_1_vectors_b);
  }
}

void InitZobrist() {
  for (auto i = 0; i < 13; ++i) for (auto j = 0; j < 64; ++j) g_zobrist_board[i][j] = Random8x64();
  for (auto i = 0; i <  2; ++i) g_zobrist_wtm[i] = Random8x64();
}

void PrintVersion() {
  std::cout << kVersion << " by Toni Helminen" << std::endl;
}

// Cain initialization (required)
void Init() {
  InitJumpMoves();
  InitZobrist();
  SetHashtable(kHashMB);
  SetFen(kStartPos);
}

void UciLoop() {
  while (Uci()) continue; // Exe UCI commands
}

} // namespace cain
