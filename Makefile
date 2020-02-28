build:
	antlr4 -no-visitor -no-listener -Dlanguage=Python3 RegexGrammar.g4

clean:
	rm RegexGrammar.interp
	rm RegexGrammar.tokens
	rm RegexGrammarLexer.interp
	rm RegexGrammarLexer.py
	rm RegexGrammarLexer.tokens
	rm RegexGrammarParser.py
