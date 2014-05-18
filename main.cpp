#include "common.h"
#include "lexer.h"
#include "term.h"


static Lexer p;

bool streq(const char *str1, const char *str2);

int main(int argc, char **argv)
{
  assert(argc >= 2);

  //disable_gc();

  string_v src_files;

	for (int i=1 ; i < argc ; i++)
	{
		char *arg = argv[i];

		ifstream ifs(arg, ifstream::in);

		while (!ifs.eof())
		{
			string s;
			getline(ifs, s);
			src_files.push_back(s);
		}
	}

  for (unsigned int i=0 ; i < src_files.size() ; i++)
  {
		cout << "Processing file " << src_files[i] << endl;

    int line_num = 0;
		ifstream ifs(src_files[i].c_str(), ifstream::in);

    if (ifs.fail())
    {
      cout << "Cannot open file " << src_files[i] << endl;
      return 1;
    }

		while (!ifs.eof())
		{
			string s;
			getline(ifs, s);
		  p.consume_line(s, src_files[i].c_str(), line_num++);
		}
  }

  int yyparse();

  if (yyparse() != 0)
  {
    getchar();
    return 1;
  }

  cerr << "Parsing successful" << endl;

  extern Term all_decls;

  //all_decls.print(cout);

  cerr << "Done!" << endl;
  getchar();
}

static unsigned int next_idx = 0;

void yyerror(char const *s)
{
  if (next_idx > 0)
  {
    int l = p.tokens[next_idx-1].start_line;
    int c = p.tokens[next_idx-1].start_col;
    const char *f = p.tokens[next_idx-1].filename;

    fprintf(stderr, "%s, file %s, line %d, col %d\n", s, f, l+1, c+1);
  }
  else
  {
    fprintf(stderr, "%s, unknown position", s);
  }
}


string get_token(unsigned int idx, int &line, int &col)
{
  assert(idx == next_idx);

  if (idx >= p.tokens.size())
    return "";

  next_idx = idx + 1;

  Token t = p.tokens[idx];

  line = t.start_line;
  col = t.start_col;

	return t.str;
}
