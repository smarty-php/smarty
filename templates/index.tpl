{* Smarty *}
BEGIN SMARTY SMOKE TEST


plain variable $foo
-------------------

$foo                                      {$foo}
$foo|upper                                {$foo|upper}
$foo|upper|spacify                        {$foo|upper|spacify}
$foo|spacify:"^^"                         {$foo|spacify:"^^"}
$foo|@count                               {$foo|@count}
$foo|default:"no"                         {$foo|default:"no"}
$foo|truncate:6:"...":true                {$foo|truncate:6:"...":true}

associative variable $afoo
--------------------------

$afoo.one                                 {$afoo.one}
$afoo.two                                 {$afoo.two}
$afoo.one|upper                           {$afoo.one|upper}
$afoo.one|upper|spacify                   {$afoo.one|upper|spacify}
$afoo.one|spacify:"^^"                    {$afoo.one|spacify:"^^"}
$afoo.one|@count                          {$afoo.one|@count}
$afoo.one|default:"no"                    {$afoo.one|default:"no"}
$afoo.one|truncate:6:"...":true           {$afoo.one|truncate:6:"...":true}
$afoo.$bar                                {$afoo.$bar}

{config_load file="test.conf"}

config variable #foo#
---------------------

#foo#                                      {#foo#}
#foo#|upper                                {#foo#|upper}
#foo#|upper|spacify                        {#foo#|upper|spacify}
#foo#|spacify:"^^"                         {#foo#|spacify:"^^"}
#foo#|@count                               {#foo#|@count}
#foo#|default:"no"                         {#foo#|default:"no"}
#foo#|truncate:6:"...":true                {#foo#|truncate:6:"...":true}

{config_load file="test.conf" section="foo"}

config variable #foo# from section foo
--------------------------------------

#foo#                                      {#foo#}
#foo#|upper                                {#foo#|upper}
#foo#|upper|spacify                        {#foo#|upper|spacify}
#foo#|spacify:"^^"                         {#foo#|spacify:"^^"}
#foo#|@count                               {#foo#|@count}
#foo#|default:"no"                         {#foo#|default:"no"}
#foo#|truncate:6:"...":true                {#foo#|truncate:6:"...":true}

object $ofoo
------------

$ofoo->blah                               {$ofoo->blah}
$ofoo->foo('bar')                         {$ofoo->foo('bar')}
$ofoo->foo("bar")                         {$ofoo->foo("bar")}
$ofoo->foo("$foo bar")                    {$ofoo->foo("$foo bar")}
$ofoo->foo("one","two")                   {$ofoo->foo("one","two")}
$ofoo->foo("one","two","three")           {$ofoo->foo("one","two","three")}
$ofoo->foo("one $foo","two","three")      {$ofoo->foo("one $foo","two","three")}
$ofoo->foo("one $foo","two $foo","three") {$ofoo->foo("one $foo","two $foo","three")}
$ofoo->foo("one","two","three")           {$ofoo->foo("one","two","three","four")}
$ofoo->foo('one $foo','two $foo','three') {$ofoo->foo('one $foo','two $foo','three')}
$ofoo->foo('one')|upper                   {$ofoo->foo('one')|upper}


nested object nfoo
------------------


$nfoo->ofoo->blah                               {$nfoo->ofoo->blah}
$nfoo->ofoo->foo('bar')                         {$nfoo->ofoo->foo('bar')}
$nfoo->ofoo->foo("bar")                         {$nfoo->ofoo->foo("bar")}
$nfoo->ofoo->foo("$foo bar")                    {$nfoo->ofoo->foo("$foo bar")}
$nfoo->ofoo->foo("one","two")                   {$nfoo->ofoo->foo("one","two")}
$nfoo->ofoo->foo("one","two","three")           {$nfoo->ofoo->foo("one","two","three")}
$nfoo->ofoo->foo("one $foo","two","three")      {$nfoo->ofoo->foo("one $foo","two","three")}
$nfoo->ofoo->foo("one $foo","two $foo","three") {$nfoo->ofoo->foo("one $foo","two $foo","three")}
$nfoo->ofoo->foo("one","two","three")           {$nfoo->ofoo->foo("one","two","three","four")}
$nfoo->ofoo->foo('one $foo','two $foo','three') {$nfoo->ofoo->foo('one $foo','two $foo','three')}
$nfoo->ofoo->foo('one')|upper                   {$nfoo->ofoo->foo('one')|upper}


function ffoo
-------------

ffoo var="foo"                            {ffoo var="foo"}
ffoo var="foo" var2="blah"                {ffoo var="foo" var2="blah"}
ffoo var=$foo                             {ffoo var=$foo}
ffoo var=truey                            {ffoo var=truey}
ffoo var=$foo|upper                       {ffoo var=$foo|upper}
ffoo var="foo"|upper                      {ffoo var="foo"|upper}
ffoo var=$ofoo->foo("$foo bar")           {ffoo var=$ofoo->foo("$foo bar")}
ffoo|upper var=$foo                       {ffoo|upper var=$foo}
 


section loop over sfoo
----------------------

{section name=sec loop=$sfoo}
$sfoo[sec]                                {$sfoo[sec]}
$sfoo[sec.index]                          {$sfoo[sec.index]}
$sfoo[sec.index_next]                     {$sfoo[sec.index_next]}
$sfoo[sec.index_prev]                     {$sfoo[sec.index_prev]}
$sfoo[sec.iteration]                      {$sfoo[sec.iteration]}
$sfoo[sec.first]                          {$sfoo[sec.first]}
$sfoo[sec.last]                           {$sfoo[sec.last]}
$sfoo[sec.rownum]                         {$sfoo[sec.rownum]}
$sfoo[sec.loop]                           {$sfoo[sec.loop]}
$sfoo[sec.total]                          {$sfoo[sec.total]}
$sfoo[sec.show]                           {$sfoo[sec.show]}
$sfoo[sec]|upper                          {$sfoo[sec]|upper}
$sfoo[sec]|upper|spacify                  {$sfoo[sec]|upper|spacify}
$sfoo[sec]|spacify:"^^"                   {$sfoo[sec]|spacify:"^^"}
$sfoo[sec]|@count                         {$sfoo[sec]|@count}
$sfoo[sec]|default:"no"                   {$sfoo[sec]|default:"no"}
$sfoo[sec]|truncate:6:"...":true          {$sfoo[sec]|truncate:6:"...":true}

$smarty.section.sec.index                 {$smarty.section.sec.index}

{/section}

if statement parse test
-----------------------

if $foo                                   {if $foo}{/if}(end)
if $foo eq "bar"                          {if $foo eq "bar"}foo is bar{/if}(end)
if is_array($foo)                         {if is_array($foo)}foo is array{else}foo is not array{/if}(end)
if $foo gt 4                              {if $foo gt 4}$foo is gt 4{/if}(end)
if ($foo gt 4)                            {if ($foo gt 4)}$foo is gt 4{/if}(end)
if ( $foo gt 4 )                          {if ( $foo gt 4 )}$foo is gt 4{/if}(end)
if $foo and not $bar                      {if $foo and not $bar}foo and not bar{/if}(end)
if !$bfoo                                 {if !$bfoo}not bfoo{else}bfoo{/if}(end)
if 4 is div by 4                          {if 4 is div by 4}div by 4{else}not div by 4{/if}(end)
if 3 is div by 4                          {if 3 is div by 4}div by 4{else}not div by 4{/if}(end)
if 3 is div by 4 or ( 3 eq 3 )            {if 3 is div by 4 or ( 3 eq 3 )}blah{/if}(end)
if (3 is div by 4) or ( 3<="bah"|upper )  {if (3 is div by 4) or ( 3<="bah"|upper )}woohoo{/if}(end)
if in_array("my $bar", $afoo)             {if in_array("my $bar", $afoo)}is in{else}is not in{/if}(end)
if $ofoo->foo()                           {if $ofoo->foo()}is in{else}is not in{/if}(end)
if in_array($ofoo->foo("two"), $afoo)     {if in_array($ofoo->foo("two"), $afoo)}is in{else}is not in{/if}(end)

$smarty variable access
-----------------------

$smarty.now                               {$smarty.now}
$smarty.const._MY_CONST                   {$smarty.const._MY_CONST}

block function
--------------

{reverse}
	This is my block of text.
{/reverse|upper}

misc syntax, try to break
-------------------------

$foo|spacify:"|"                          {$foo|spacify:"|"}
$foo|spacify:"|"|upper                    {$foo|spacify:"|"|upper}
$foo|spacify:"$foo"|upper|default:"|"     {$foo|spacify:"$foo"|upper|default:"|"}

{section name=sec loop=$sfoo}
$ofoo->foo($sfoo[sec],$sfoo[sec]|spacify:"^^",$sfoo[sec]|truncate:6:"...":true) {$ofoo->foo($sfoo[sec],$sfoo[sec]|spacify:"^^",$sfoo[sec]|truncate:6:"...":true)} 
$nfoo->ofoo->foo($sfoo[sec],$sfoo[sec]|spacify:"^^",$sfoo[sec]|truncate:6:"...":true) {$nfoo->ofoo->foo($sfoo[sec],$sfoo[sec]|spacify:"^^",$sfoo[sec]|truncate:6:"...":true)} 
{/section}

ffoo|spacify:"|"|upper var="lalala"       {ffoo|spacify:"|"|upper var="lalala"}
ffoo|spacify:" my spaces "|upper var="lalala"       {ffoo|spacify:" my spaces "|upper var="lalala"}
ffoo|spacify:"!@#$%^&*()_+=-\|]["|upper var="lalala"       {ffoo|spacify:"!@#$%^&*()_+=-\|]["|upper var="lalala"}

{config_load file=test.conf section=foo}

if !$nfoo->ofoo->foo("blah blah")         {if !$nfoo->ofoo->foo("blah blah")}not blah{else}blah{/if}(end)

"static"|upper                            {"static"|upper}

{foreach item="fofoo" from=$afoo}

DEBUG FOREACH: {$fofoo}

{/foreach}

END SMARTY SMOKE TEST

