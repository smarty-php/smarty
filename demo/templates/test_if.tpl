Test of if tags <br>
{foreach item=value from=$values}
Value of {$value} is {if $value<10} less then 10 {elseif ($value GT 100) AND ($value <= 500)} something between 100 and 500 {else} somthing else {/if}<br>
{/foreach}

