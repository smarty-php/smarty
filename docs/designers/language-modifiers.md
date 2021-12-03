Variable Modifiers {#language.modifiers}
==================

Variable modifiers can be applied to
[variables](#language.syntax.variables), [custom
functions](#language.custom.functions) or strings. To apply a modifier,
specify the value followed by a `|` (pipe) and the modifier name. A
modifier may accept additional parameters that affect its behavior.
These parameters follow the modifier name and are separated by a `:`
(colon). Also, *all php-functions can be used as modifiers implicitly*
(more below) and modifiers can be
[combined](#language.combining.modifiers).


    {* apply modifier to a variable *}
    {$title|upper}

    {* modifier with parameters *}
    {$title|truncate:40:"..."}

    {* apply modifier to a function parameter *}
    {html_table loop=$myvar|upper}

    {* with parameters *}
    {html_table loop=$myvar|truncate:40:"..."}

    {* apply modifier to literal string *}
    {"foobar"|upper}

    {* using date_format to format the current date *}
    {$smarty.now|date_format:"%Y/%m/%d"}

    {* apply modifier to a custom function *}
    {mailto|upper address="smarty@example.com"}

    {* using  php's str_repeat *}
    {"="|str_repeat:80}

    {* php's count *}
    {$myArray|@count}

    {* this will uppercase and truncate the whole array *}
    <select name="name_id">
    {html_options output=$my_array|upper|truncate:20}
    </select>

      

-   Modifiers can be applied to any type of variables, including arrays
    and objects.

    > **Note**
    >
    > The default behavior was changed with Smarty 3. In Smarty 2.x, you
    > had to use an \"`@`\" symbol to apply a modifier to an array, such
    > as `{$articleTitle|@count}`. With Smarty 3, the \"`@`\" is no
    > longer necessary, and is ignored.
    >
    > If you want a modifier to apply to each individual item of an
    > array, you will either need to loop the array in the template, or
    > provide for this functionality inside your modifier function.

    > **Note**
    >
    > Second, in Smarty 2.x, modifiers were applied to the result of
    > math expressions like `{8+2}`, meaning that
    > `{8+2|count_characters}` would give `2`, as 8+2=10 and 10 is two
    > characters long. With Smarty 3, modifiers are applied to the
    > variables or atomic expressions before executing the calculations,
    > so since 2 is one character long, `{8+2|count_characters}`
    > gives 9. To get the old result use parentheses like
    > `{(8+2)|count_characters}`.

-   Modifiers are autoloaded from the
    [`$plugins_dir`](#variable.plugins.dir) or can be registered
    explicitly with the [`registerPlugin()`](#api.register.plugin)
    function. The later is useful for sharing a function between php
    scripts and smarty templates.

-   All php-functions can be used as modifiers implicitly, as
    demonstrated in the example above. However, using php-functions as
    modifiers has two little pitfalls:

    -   First - sometimes the order of the function-parameters is not
        the desirable one. Formatting `$foo` with
        `{"%2.f"|sprintf:$foo}` actually works, but asks for the more
        intuitive, like `{$foo|string_format:"%2.f"}` that is provided
        by the Smarty distribution.

    -   Secondly - if security is enabled, all php-functions that are to
        be used as modifiers have to be declared trusted in the
        `$modifiers` property of the securty policy. See the
        [Security](#advanced.features.security) section for details.

See also [`registerPlugin()`](#api.register.plugin), [combining
modifiers](#language.combining.modifiers). and [extending smarty with
plugins](#plugins)

DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-CAPITALIZE
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-CAT
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-COUNT-CHARACTERS
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-COUNT-PARAGRAPHS
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-COUNT-SENTENCES
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-COUNT-WORDS
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-DATE-FORMAT
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-DEFAULT
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-ESCAPE
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-FROM-CHARSET
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-INDENT
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-LOWER
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-NL2BR
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-REGEX-REPLACE
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-REPLACE
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-SPACIFY
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-STRING-FORMAT
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-STRIP
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-STRIP-TAGS
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-TO-CHARSET
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-TRUNCATE
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-UNESCAPE
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-UPPER
DESIGNERS.LANGUAGE-MODIFIERS.LANGUAGE-MODIFIER-WORDWRAP
