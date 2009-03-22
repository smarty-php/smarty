
Test methode chaining
{assign var=x value=33}
<br>{assign var=x2  value=10}{$person->object->setName('peter')->setAge($x+4)->introduce()}
<br>{$person->object->setAge($x+$x2)->setName('paul')->introduce()}

