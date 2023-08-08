# Variable Modifiers

Variable modifiers can be applied to
[variables](../language-variables/index.md), [custom tags](../language-custom-functions/index.md)
or strings. To apply a modifier,
specify the value followed by a `|` (pipe) and the modifier name. A
modifier may accept additional parameters that affect its behavior.
These parameters follow the modifier name and are separated by a `:`
(colon). Also, *all php-functions can be used as modifiers implicitly*
(more below) and modifiers can be
[combined](../language-combining-modifiers.md).

- [capitalize](language-modifier-capitalize.md)
- [cat](language-modifier-cat.md)
- [count_characters](language-modifier-count-characters.md)
- [count_paragraphs](language-modifier-count-paragraphs.md)
- [count_sentences](language-modifier-count-sentences.md)
- [count_words](language-modifier-count-words.md)
- [date_format](language-modifier-date-format.md)
- [default](language-modifier-default.md)
- [escape](language-modifier-escape.md)
- [from_charset](language-modifier-from-charset.md)
- [indent](language-modifier-indent.md)
- [lower](language-modifier-lower.md)
- [nl2br](language-modifier-nl2br.md)
- [regex_replace](language-modifier-regex-replace.md)
- [replace](language-modifier-replace.md)
- [spacify](language-modifier-spacify.md)
- [string_format](language-modifier-string-format.md)
- [strip](language-modifier-strip.md)
- [strip_tags](language-modifier-strip-tags.md)
- [to_charset](language-modifier-to-charset.md)
- [truncate](language-modifier-truncate.md)
- [unescape](language-modifier-unescape.md)
- [upper](language-modifier-upper.md)
- [wordwrap](language-modifier-wordwrap.md)

## Examples

```smarty
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
```  
      
- Modifiers can be applied to any type of variables, including arrays
    and objects.

    > **Note**
    >
    > The default behavior was changed with Smarty 3. In Smarty 2.x, you
    > had to use an "`@`" symbol to apply a modifier to an array, such
    > as `{$articleTitle|@count}`. With Smarty 3, the "`@`" is no
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

- Custom modifiers can be registered
    with the [`registerPlugin()`](../../programmers/api-functions/api-register-plugin.md)
    function.

See also [`registerPlugin()`](../../programmers/api-functions/api-register-plugin.md), [combining
modifiers](../language-combining-modifiers.md). and [extending smarty with
plugins](../../api/extending/introduction.md)
