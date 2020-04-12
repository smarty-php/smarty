{strip}
{assign var=samplearr value=[["list" => []],["list" => ["item"]]]}
{foreach $samplearr as $v}
    {section name=inner loop=$v.list}
    {/section}
    loop: {$v@iteration}
    inner: {$smarty.section.inner.total}
{/foreach}