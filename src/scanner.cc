#include <cwctype>
#include <tree_sitter/parser.h>
#include <stdio.h>

namespace {

using std::iswspace;

enum TokenType {
  COMMENT,
  STRING,
  PREPROCESS_START,
  PREPROCESS_CONTENT,
  PREPROCESS_END,
};

struct Scanner {
  int level;

  static bool eof(TSLexer *lexer) { return lexer->eof(lexer); }
  static void mark_end(TSLexer *lexer) { lexer->mark_end(lexer); }

  static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

  static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

  static bool scan_sequence(TSLexer *lexer, const char *sequence) {
    // Try to match all characters in the given 'sequence'
    for (const char *c = sequence; *c; c++) {
      if (lexer->lookahead == *c) {
        // Consume the character in 'c'
        advance(lexer);
      } else {
        return false;
      }
    }

    return true;
  }

  static bool scan_multiline_content(TSLexer *lexer) {
    // Initialize lua multiline content level count
    int start_level = 0;
    int end_level = 0;

    if (lexer->lookahead == '[') {
      // Consume first appearance of '['
      advance(lexer);

      if (lexer->lookahead == '[' || lexer->lookahead == '=') {
        while (lexer->lookahead == '=') {
          // Increment level count
          ++start_level;

          // Consume all '=' characters
          advance(lexer);
        }

        if (lexer->lookahead == '[') {
          // Consume last appearance of '['
          advance(lexer);

          // Loop while not end of file (eof)
          while (lexer->lookahead != 0) {
            // Gives the end level count the same as start level count
            end_level = start_level;

            if (lexer->lookahead == ']') {
              // Consume first appearance of ']'
              advance(lexer);

              if (lexer->lookahead == ']' || lexer->lookahead == '=') {
                while (lexer->lookahead == '=' && end_level > 0) {
                  // Decrement level count
                  --end_level;

                  // Consume all '=' characters
                  advance(lexer);
                }

                if (lexer->lookahead == ']' && end_level == 0) {
                  // Consume last appearance of ']'
                  advance(lexer);

                  return true;
                }
              }
            }

            if (lexer->lookahead != 0) {
              // Consume all but end of file (eof)
              advance(lexer);
            }
          }
        }
      }
    }

    return false;
  }

  bool scan(TSLexer *lexer, const bool *valid_symbols) {    
    if (valid_symbols[PREPROCESS_END]) {
      if (level == -1) {
        if (lexer->lookahead == '\n') {
          advance(lexer);
        }
        lexer->result_symbol = PREPROCESS_END;
        return true;
      } else {
        if (lexer->lookahead != ']')
          return false;
        advance(lexer);
        for (int i = 0; i < level; i++) {
          if (lexer->lookahead != '=')
            return false;
          advance(lexer);
        }
        if (lexer->lookahead != ']')
          return false;
        advance(lexer);
        lexer->result_symbol = PREPROCESS_END;
        return true;
      }
    }
    while (iswspace(lexer->lookahead)) {
      skip(lexer);
    }
    if (valid_symbols[PREPROCESS_CONTENT]) {
      if (level == -1) {
        while (lexer->lookahead != '\n' && !eof(lexer)) {
          advance(lexer);
        }
        lexer->result_symbol = PREPROCESS_CONTENT;
        return true;
      } else {
        int end_level = level;
        while (!eof(lexer)) {
          if (lexer->lookahead == ']') {
            mark_end(lexer);
            advance(lexer);
            if (lexer->lookahead == ']' || lexer->lookahead == '=') {
              while (lexer->lookahead == '=' && end_level > 0) {
                --end_level;
                advance(lexer);
              }

              if (lexer->lookahead == ']' && end_level == 0) {
                advance(lexer);
                lexer->result_symbol = PREPROCESS_CONTENT;
                return true;
              }
            }
          } else {
            advance(lexer);
          }
        }
      }
    }
    if (valid_symbols[PREPROCESS_START]) {
      if (scan_sequence(lexer, "##")) {
        if (lexer->lookahead == '[') {
          advance(lexer);
          level = 0;
          if (lexer->lookahead == '[' || lexer->lookahead == '=') {
            while (lexer->lookahead == '=') {
              ++level;
              advance(lexer);
            }
            if (lexer->lookahead == '[') {
              advance(lexer);
              lexer->result_symbol = PREPROCESS_START;
              return true;
            }
          }
        } else {
          level = -1;
          lexer->result_symbol = PREPROCESS_START;
          return true;
        }
      }
    }
    if (valid_symbols[COMMENT] || valid_symbols[STRING]) {
      // Try to make a short literal string with single quote
      if (lexer->lookahead == '\'') {
        lexer->result_symbol = STRING;

        // Consume first appearance of '\''
        advance(lexer);

        // Loop when isn't new line neither end of file (eof)
        while (lexer->lookahead != '\n' && lexer->lookahead != 0) {
          if (lexer->lookahead == '\\') {
            // Consume '\\'
            advance(lexer);

            if (lexer->lookahead != '\n' && lexer->lookahead != 0) {
              // Consume any character that isn't new line neither end of file
              // (eof)
              advance(lexer);
            } else {
              break;
            }
          } else {
            if (lexer->lookahead == '\'') {
              // Consume last appearance of '\''
              advance(lexer);

              return true;
            } else {
              if (lexer->lookahead != '\n' && lexer->lookahead != 0) {
                // Consume any character that isn't new line neither end of file
                // (eof)
                advance(lexer);
              } else {
                break;
              }
            }
          }
        }
      }

      // Try to make a short literal string with double quote
      else if (lexer->lookahead == '"') {
        lexer->result_symbol = STRING;

        // Consume first appearance of '"'
        advance(lexer);

        // Loop when next character isn't new line neither end of file (eof)
        while (lexer->lookahead != '\n' && lexer->lookahead != 0) {
          if (lexer->lookahead == '\\') {
            // Consume '\\'
            advance(lexer);

            if (lexer->lookahead != '\n' && lexer->lookahead != 0) {
              // Consume any character that isn't new line neither end of file
              // (eof)
              advance(lexer);
            } else {
              break;
            }
          } else {
            if (lexer->lookahead == '"') {
              // Consume last appearance of '"'
              advance(lexer);

              return true;
            } else {
              if (lexer->lookahead != '\n' && lexer->lookahead != 0) {
                // Consume any character that isn't new line neither end of file
                // (eof)
                advance(lexer);
              } else {
                break;
              }
            }
          }
        }
      }

      // Try to make a comment
      else if (scan_sequence(lexer, "--")) {
        if (scan_multiline_content(lexer)) {
          lexer->result_symbol = COMMENT;
          return true;
        }

        while (iswspace(lexer->lookahead) && lexer->lookahead != '\n' &&
               lexer->lookahead != 0) {
          advance(lexer);
        }

        lexer->result_symbol = COMMENT;

        while (lexer->lookahead != '\n' && lexer->lookahead != 0) {
          // Consume any character that isn't new line neither end of file (eof)
          advance(lexer);
        }

        return true;
      }

      // Try to make a long literal string with double bracket
      else if (scan_multiline_content(lexer)) {
        lexer->result_symbol = STRING;

        return true;
      }
    }

    return false;
  }
};

} // namespace

extern "C" {

void *tree_sitter_nelua_external_scanner_create() { return new Scanner(); }

void tree_sitter_nelua_external_scanner_destroy(void *payload) {
  Scanner *scanner = static_cast<Scanner *>(payload);
  delete scanner;
}

bool tree_sitter_nelua_external_scanner_scan(void *payload, TSLexer *lexer,
                                             const bool *valid_symbols) {
  Scanner *scanner = static_cast<Scanner *>(payload);
  return scanner->scan(lexer, valid_symbols);
}

unsigned tree_sitter_nelua_external_scanner_serialize(void *payload,
                                                      char *buffer) {
  return 0;
}

void tree_sitter_nelua_external_scanner_deserialize(void *payload,
                                                    const char *buffer,
                                                    unsigned length) {}
}