{foreach $bar as $i}
    {$i}{make_nocache $i}{if $i == $foo}match{/if}
{/foreach}