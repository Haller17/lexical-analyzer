package Spring2026;

/**

This class is a lexical analyzer for the tokens defined by the grammar:

IDENT		
INT		
FLOAT		
ASSIGN		<-
LPAREN		(
RPAREN		)
ADD		+
SUB		-
MUL		*
DIV		/
COMMA		,
EQ		=
NEQ		/=
LT		<
GT		>
LE		<=
GE		>=
LBRACE		{
RBRACE		}


This class implements a DFA that will accept the above tokens.

The DFA states are represented by the Enum type "State".
The DFA has the following final key word states.

Keywords (all uppercase): 
DISPLAY, INPUT, MOD, RANDOM, NOT, AND, OR, IF, ELSE, REPEAT, TIMES, UNTIL, PROCEDURE, RETURN


The DFA also uses the following 3 non-final states:

START			the empty string
DOT				.
UNDEFINED


The function "driver" operates the DFA. 
The array "nextState" returns the next state given the current state and the input character.

 **/


public class LexerExample extends IO
{
	public enum State
	{

			START,

			DOT, //middle state (non-final)


			IDENT,
			FLOAT,
			INT,
			ASSIGN,
			LPAREN,
			RPAREN,
			ADD,
			SUB,
			MUL,
			DIV,
			COMMA,
			EQ,
			NEQ,
			LT,
			GT,
			LE,
			GE,
			LBRACE,
			RBRACE,

			UNDEFINED;



		private boolean isFinal()
		{
			return this != State.START && this != State.UNDEFINED && this != State.DOT;
		}



	}

	// By enumerating the non-final states first and then the final states,
	// test for a final state can be done by testing if the state's ordinal number
	// is greater than or equal to that of Id.

	// The following variables of "Spring2026.IO" class are used:

	//   static int a; the current input character
	//   static char c; used to convert the variable "a" to the char type whenever necessary

	public static String t; // holds an extracted token
	public static State state; // the current state of the FA

	public static String[] keywords = {"DISPLAY","INPUT","MOD","RANDOM","NOT","AND","OR",
			"IF","ELSE","REPEAT","TIMES","UNTIL","PROCEDURE","RETURN"};


	private static State nextState[][] = new State[19][128];

	// This array implements the state transition function State x (ASCII char set) --> State.
	// The state argument is converted to its ordinal number used as
	// the first array index from 0 through 13. 

	private static int driver()

	// This is the driver of the FA. 
	// If a valid token is found, assigns it to "t" and returns 1.
	// If an invalid token is found, assigns it to "t" and returns 0.
	// If end-of-stream is reached without finding any non-whitespace character, returns -1.

	{
		State nextSt; // the next state of the FA

		t = "";
		state = State.START;

		if ( Character.isWhitespace((char) a) )
			a = getChar(); // get the next non-whitespace character
		if ( a == -1 ) // end-of-stream is reached
			return -1;

		while ( a != -1 ) // do the body if "a" is not end-of-stream
		{
			c = (char) a;
			nextSt = nextState[state.ordinal()][a];
			if ( nextSt == State.UNDEFINED ) // The FA will halt.
			{
				if ( state.isFinal() )
					return 1; // valid token extracted
				else // "c" is an unexpected character
				{
					t = t+c;
					a = getNextChar();
					return 0; // invalid token found
				}
			}
			else // The FA will go on.
			{
				state = nextSt;
				t = t+c;
				a = getNextChar();
			}
		}

		// end-of-stream is reached while a token is being extracted

		if ( state.isFinal() )
			return 1; // valid token extracted
		else
			return 0; // invalid token found
	} // end driver

	private static void setNextState() {
		for (int s = 0; s < nextState.length; s++)
			for (int c = 0; c < nextState[0].length; c++)
				nextState[s][c] = State.UNDEFINED;


		for (char c = 'A'; c <= 'Z'; c++) {
			nextState[State.START.ordinal()][c] = State.IDENT;
			nextState[State.IDENT.ordinal()][c] = State.IDENT;
		}
		for (char c = 'a'; c <= 'z'; c++) {
			nextState[State.START.ordinal()][c] = State.IDENT;
			nextState[State.IDENT.ordinal()][c] = State.IDENT;
		}
		for (char c = '0'; c <= '9'; c++) {
			nextState[State.IDENT.ordinal()][c] = State.IDENT;
		}

		for (char c = '0'; c <= '9'; c++) {
			nextState[State.START.ordinal()][c] = State.INT;
			nextState[State.INT.ordinal()][c]   = State.INT;
		}

		nextState[State.START.ordinal()]['.'] = State.DOT; //Not valid ending state
		nextState[State.INT.ordinal()]['.']   = State.DOT; //Not valid ending state

		for (char c = '0'; c <= '9'; c++) {
			nextState[State.DOT.ordinal()][c]   = State.FLOAT;
			nextState[State.FLOAT.ordinal()][c] = State.FLOAT;
		}

		nextState[State.START.ordinal()]['+'] = State.ADD;
		nextState[State.START.ordinal()]['-'] = State.SUB;
		nextState[State.START.ordinal()]['*'] = State.MUL;
		nextState[State.START.ordinal()]['/'] = State.DIV;
		nextState[State.START.ordinal()]['('] = State.LPAREN;
		nextState[State.START.ordinal()][')'] = State.RPAREN;
		nextState[State.START.ordinal()][','] = State.COMMA;
		nextState[State.START.ordinal()]['='] = State.EQ;
		nextState[State.START.ordinal()]['{'] = State.LBRACE;
		nextState[State.START.ordinal()]['}'] = State.RBRACE;

		nextState[State.START.ordinal()]['<'] = State.LT;
		nextState[State.START.ordinal()]['>'] = State.GT;

		nextState[State.DIV.ordinal()]['='] = State.NEQ;
		nextState[State.LT.ordinal()]['=']  = State.LE;
		nextState[State.LT.ordinal()]['-']  = State.ASSIGN;
		nextState[State.GT.ordinal()]['=']  = State.GE;
	}


	// end setNextState

	public static void setLex()

	// Sets the nextState array.

	{
		setNextState();
	}

	public static void main(String argv[])

	{		
		// argv[0]: input file containing source code using tokens defined above
		// argv[1]: output file displaying a list of the tokens 

		setIO( argv[0], argv[1] );
		setLex();

		int i;

		while ( a != -1 ) // while "a" is not end-of-stream
		{
			i = driver(); // extract the next token
			if ( i == 1 && state.equals(State.IDENT) ){
				boolean keyword = false;
				for (String key : keywords){
					if (t.equals(key)){
						displayln( t+"\t: Keyword_"+t );
						keyword = true;
					}
				}
				if (!keyword)
					displayln( t+"\t: "+state.toString() );
			}
			else if ( i == 1 )
				displayln( t+"\t: "+state.toString() );
			else if ( i == 0 )
				displayln( t+"\t: Lexical Error, invalid token");
		} 

		closeIO();
	}
} 

