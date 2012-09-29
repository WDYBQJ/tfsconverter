/*
 * 2012-09-26  jiangqb08  <jiangqb08@jiangqb08-IdeaPad-Y430>

 * 
 */
#include <cstdio>
#include <cstring>
#include <cctype>
#include <climits>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include <cstdarg>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <exception>
#include <stdexcept>
#include <memory>
#include <locale>
#include <bitset>
#include <deque>
#include <list>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <vector>
#include <algorithm>
#include <iterator>
#include <functional>
#include <string>
#include <complex>
#include <valarray>

using namespace std;

template <class T> inline T checkmin(T &a, T b)
{
	return (a < b) ? a : a = b;
}

template <class T> inline T checkmax(T &a, T b)
{
	return (a > b) ? a : a = b;
}

template <class T> T GCD(T a, T b)
{
	if (a < 0)
		return GCD(- a, b);
	if (b < 0)
		return GCD(a, - b);
	return (a == 0) ? b : GCD(b % a, a);
}

template <class T> T LCM(T a, T b)
{
	if (a < 0)
		return LCM(- a, b);
	if (b < 0)
		return LCM(a, - b);
	return (a == 0 || b == 0) ? 0 : a / GCD(a, b) * b;
}

template <class T> T ExtGCD(T a, T b, T &x, T &y)
{
	if (a < 0)
	{
		T c = ExtGCD(- a, b, x, y);
		x = - x;
		return c;
	}
	if (b < 0)
	{
		T c = ExtGCD(a, - b, x, y);
		y = - y;
		return c;
	}
	if (a == 0)
	{
		x = 0, y = 1;
		return b;
	}
	else
	{
		T c = ExtGCD(b % a, a, y, x);
		x -= b / a * y;
		return c;
	}
}

template <class T> inline T sqr(T X)
{
	return X * X;
}

#define tr(i, x) for (typeof(x.begin()) i = x.begin(); i != x.end(); ++ i)
#define rep(i, n) for (int i = 0; i < n; ++ i)
#define pii pair <int, int>
#define mp make_pair
#define pb push_back
#define x first
#define y second
#define ll long long
#define cmplxd complex <long double>
#define pi 3.14159265358979323846264338327950288

class TFGenePiece
{
	private:
		
	public:
		int Begin, End, Direction; // Interval: [Begin, End). Direction = - 1 when <---, = 1 when --->, = 0 when ----.
		double Score;
		string ID, Name;
		bool IsHomo()
		{
			return CheckHomo(Download());
		}
		bool CanCombine(TFGenePiece &X)
		{
			return End == X.Begin && ((Direction == - 1 && X.Direction == 0) || (Direction == 0 && X.Direction == 1)) && Score == X.Score && ID == X.ID && Name == X.Name;
		}
		void Combine(TFGenePiece &X)
		{
			End = X.End;
			Direction += X.Direction;
		}
		string Download()
		{
			string FileName = "tmp/" + ID;
			system(("wget -nc -O " + FileName + " http://www.cbrc.jp/htbin/bget_tfmatrix\?" + ID).c_str());
			return FileName;
		}
		bool CheckHomo(string FileName)
		{
			FILE *fn = fopen(FileName.c_str(), "r");
			deque <char> Q;
			char Ch;
			string St = "";
			while (fscanf(fn, "%c", &Ch) != EOF)
				St += Ch;
			fclose(fn);
			return (St.find("Homo") != string::npos);
		}
};

class TFTextCoverter
{
	private:
		static const int MaxLineLength = 256;
		static const int MaxTotalLength = 131072;
	public:
		void Convert(FILE *fi, FILE *fo)
		{
			char *Full = new char[MaxTotalLength];
			char *FirstLine = new char[MaxLineLength];
			char *NextLine = new char[MaxLineLength];
			int *Label = new int[MaxLineLength];
			int LenFull = 0;
			Full[0] = '\0';
			vector <TFGenePiece> All, NewAdded;
			All.clear();
			NewAdded.clear();
			while (fgets(NextLine, MaxLineLength, fi))
			{
				if (Meaningless(NextLine))
				{
					vector <TFGenePiece> Tmp;
					Tmp.clear();
					while (!NewAdded.empty())
					{
						TFGenePiece Cur = NewAdded.back();
						NewAdded.pop_back();
						if (Cur.IsHomo())
						{
							bool Flag = 1;
							for (int i = (int) All.size() - 1; i >= 0 && Flag; -- i)
								if (All[i].CanCombine(Cur))
								{
									Flag = 0;
									All[i].Combine(Cur);
								}
							if (Flag)
								Tmp.pb(Cur);
						}
					}
					while (!Tmp.empty())
					{
						All.pb(Tmp.back());
						Tmp.pop_back();
					}
					continue;
				}
				if (IsFirstLine(NextLine))
				{
					Copy(NextLine, FirstLine);
					AnalysisFirstLine(FirstLine, Label, Full, LenFull);
				}
				else
					NewAdded.pb(AnalysisGeneralLine(NextLine, Label));
			}
			fputs(Full, fo);
			fprintf(fo, "\n%d\n", (int) All.size());
			tr (it, All)
				fprintf(fo, "%d %d %d %.1lf %s %s\n", it->Begin, it->End, it->Direction, it->Score, it->ID.c_str(), it->Name.c_str());
			delete[] Full;
			delete[] FirstLine;
			delete[] NextLine;
			delete[] Label;
			
		}
		bool IsFirstLine(char *St)
		{
			int Len = strlen(St);
			rep (i, Len)
			{
				if (isdigit(St[i]))
					return 1;
				if (isalpha(St[i]))
					return 0;
				if (St[i] == '-' || St[i] == '<' || St[i] == '>')
					return 0;
			}
			return 0;
		}
		void Copy(char *St1, char *St2)
		{
			int Len = strlen(St1);
			rep (i, Len)
				St2[i] = St1[i];
			St2[Len] = '\0';
		}
		void AnalysisFirstLine(char *St, int *Pos, char *Stack, int &Top)
		{
			int Len = strlen(St);
			rep (i, Len)
			{
				if (isupper(St[i]))
				{
					Pos[i] = Top;
					Stack[Top] = St[i];
					++ Top;
				}
				else
					Pos[i] = - 1;
			}
			Stack[Top] = '\0';
		}
		TFGenePiece AnalysisGeneralLine(char *St, int *Label)
		{
			TFGenePiece Result;
			char *TmpSt = St;
			int *TmpLabel = Label;
			while (*TmpSt != '<' && *TmpSt != '>' && *TmpSt != '-')
			{
				++ TmpSt;
				++ TmpLabel;
			}
			Result.Direction = 0;
			Result.Begin = *TmpLabel;
			if (*TmpSt == '<')
				Result.Direction = - 1;
			while (*TmpSt == '<' || *TmpSt == '>' || *TmpSt == '-')
			{
				++ TmpSt;
				++ TmpLabel;
			}
			-- TmpSt;
			-- TmpLabel;
			Result.End = *TmpLabel + 1;
			if (*TmpSt == '>')
				Result.Direction = 1;
			++ TmpSt;
			char *ID = new char[MaxLineLength];
			char *Name = new char[MaxLineLength];
			double Score;
			sscanf(TmpSt, "%s%s%lf", ID, Name, &Score);
			Result.ID = ID;
			Result.Name = Name;
			Result.Score = Score;
			delete[] ID;
			delete[] Name;
			return Result;
		}
		bool Meaningless(char *St)
		{
			int Len = strlen(St);
			rep (i, Len)
				if (isdigit(St[i]) || isalpha(St[i]))
					return 0;
			return 1;
		}
};

class TFCounter
{
	private:
		static const int MaxWordLength = 16;
		static const int MaxTotalLength = 131072;
		char Model[MaxTotalLength];
		map < pair <string, string>, map < pair <string, int>, vector < pair <int, int> > > > Map;
		vector < pair < int, pair <string, string> > > BigCategory;
		vector < vector < pair < int, pair <string, int> > > > MiddleCategory;
	public:
		void Count(FILE *fp)
		{
			fgets(Model, MaxTotalLength, fp);
			Map.clear();
			int N;
			fscanf(fp, "%d", &N);
			rep (i, N)
			{
				int L, R, D;
				double S;
				char *ID = new char[MaxWordLength];
				char *Name = new char[MaxWordLength];
				fscanf(fp, "%d%d%d%lf%s%s", &L, &R, &D, &S, ID, Name);
				string Seq = "";
				for (int k = L; k < R; ++ k)
					Seq += Model[k];
				Map[mp(ID, Name)][mp(Seq, D)].pb(mp(L, R));
				delete[] ID;
				delete[] Name;
			}
			BigCategory.clear();
			tr (it, Map)
			{
				int Cnt = 0;
				tr (eit, it->y)
					Cnt += eit->y.size();
				BigCategory.pb(mp(- Cnt, it->x));
			}
			sort(BigCategory.begin(), BigCategory.end());
			MiddleCategory.clear();
			MiddleCategory.resize(BigCategory.size());
			rep (i, (int) MiddleCategory.size())
			{
				tr (it, Map[BigCategory[i].y])
					MiddleCategory[i].pb(mp(- (int) it->y.size(), it->x));
				sort(MiddleCategory[i].begin(), MiddleCategory[i].end());
			}
			tr (it, Map)
				tr (eit, it->y)
					sort(eit->y.begin(), eit->y.end());
		}
		void ExportCsv(FILE *fq)
		{
			rep (i, (int) BigCategory.size())
			{
				fprintf(fq, "%d,,%s,%s,,,\n", - BigCategory[i].x, BigCategory[i].y.x.c_str(), BigCategory[i].y.y.c_str());
				rep (j, (int) MiddleCategory[i].size())
				{
					fprintf(fq, ",%d,,,%s,%d,", - MiddleCategory[i][j].x, MiddleCategory[i][j].y.x.c_str(), MiddleCategory[i][j].y.y);
					tr (it, Map[BigCategory[i].y][MiddleCategory[i][j].y])
					{
						if (it != Map[BigCategory[i].y][MiddleCategory[i][j].y].begin())
							fprintf(fq, " ");
						if (MiddleCategory[i][j].y.y == - 1)
							fprintf(fq, "%d", it->y);
						else
							fprintf(fq, "%d", it->x + 1);
					}
					fprintf(fq, "\n");
				}
			}
		}
};

TFTextCoverter TFTC;
TFCounter TFC;

int main(int argc, char **argv)
{
	FILE *fp = fopen("input/searchresult.txt", "r");
	FILE *fq = fopen("tmp/ordered.txt", "w");
	TFTC.Convert(fp, fq);
	fclose(fp);
	fclose(fq);
	fp = fopen("tmp/ordered.txt", "r");
	fq = fopen("output/result.csv", "w");
	TFC.Count(fp);
	TFC.ExportCsv(fq);
	fclose(fp);
	fclose(fq);
	return 0;
}
