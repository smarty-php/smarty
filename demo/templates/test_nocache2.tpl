Test caching and nocache attribute
<br>cached time is {$t1}
<br>nocached time is {$t2}
<br>
{$t1+$t1} {$t1+$t2}
<br>
{nocache}
{for $i=0;$i<10;$i++}{$i}{/for}
{/nocache}
