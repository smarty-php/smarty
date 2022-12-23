all: lexers parsers

lexers: src/Lexer/ConfigfileLexer.php src/Lexer/TemplateLexer.php
parsers: src/Parser/ConfigfileParser.php src/Parser/TemplateParser.php

src/Lexer/ConfigfileLexer.php: src/Lexer/ConfigfileLexer.plex
	php ./utilities/make-lexer.php src/Lexer/ConfigfileLexer.plex src/Lexer/ConfigfileLexer.php

src/Lexer/TemplateLexer.php: src/Lexer/TemplateLexer.plex
	php ./utilities/make-lexer.php src/Lexer/TemplateLexer.plex src/Lexer/TemplateLexer.php

src/Parser/ConfigfileParser.php: src/Parser/ConfigfileParser.y
	php ./utilities/make-parser.php src/Parser/ConfigfileParser.y src/Parser/ConfigfileParser.php

src/Parser/TemplateParser.php: src/Parser/TemplateParser.y
	php ./utilities/make-parser.php src/Parser/TemplateParser.y src/Parser/TemplateParser.php

clean:
	rm -f src/Lexer/ConfigfileLexer.php src/Lexer/TemplateLexer.php src/Parser/ConfigfileParser.php src/Parser/TemplateParser.php