grammar RegexGrammar;

/*
	PARSER RULES
*/


regex               : alternation | other_regex;
alternation         : other_regex ALTERNATION regex;

other_regex         : concatenation | other_regex_2;
concatenation       : other_regex_2 other_regex;
other_regex_2       : other_regex_2 KLEENE_STAR | other_regex_2 PLUS | other_regex_2 QUESTION_MARK | app | other_regex_3;
other_regex_3       : parenthesized_regex | anything | group | DIGIT | ALPHA;
parenthesized_regex : OPEN_PARANTHESIS regex CLOSED_PARANTHESIS;
anything 			: ANYTHING;
group				: OPEN_BRACKET group_contents CLOSED_BRACKET;
group_contents      : set_item | set_item group_contents;
set_item			: interval | (DIGIT | ALPHA);
interval			: (DIGIT | ALPHA) INTERVAL (DIGIT | ALPHA);
app					: (other_regex_3 fixed_app) | (other_regex_3 max_app) | (other_regex_3 min_app) | (other_regex_3 range_app);
fixed_app 			: OPEN_CURLY_BRACKET number CLOSED_CURLY_BRACKET;
max_app				: OPEN_CURLY_BRACKET COMMA (DIGIT)+ CLOSED_CURLY_BRACKET;
min_app             : OPEN_CURLY_BRACKET number COMMA CLOSED_CURLY_BRACKET;
range_app			: OPEN_CURLY_BRACKET number COMMA number CLOSED_CURLY_BRACKET;
number				: DIGIT number | DIGIT;

/*
	LEXER RULES
*/

DIGIT : [0-9];
ALPHA : [a-zA-Z];
OPEN_BRACKET : '[';
CLOSED_BRACKET : ']';
INTERVAL: '-';
ANYTHING: '.';
ALTERNATION: '|';
KLEENE_STAR: '*';
PLUS: '+';
QUESTION_MARK: '?';
OPEN_CURLY_BRACKET: '{';
CLOSED_CURLY_BRACKET: '}';
COMMA: ',';
OPEN_PARANTHESIS: '(';
CLOSED_PARANTHESIS: ')';
