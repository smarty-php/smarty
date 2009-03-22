<?php
/**
* Test script for PHP template
* @author Monte Ohrt <monte at ohrt dot com> 
* @package SmartyTestScripts
*/
require('../libs/Smarty.class.php');
ini_set('short_open_tag','1');

 class Person
{
    private $m_szName;
    private $m_iAge;
    
    public function setName($szName)
    {
        $this->m_szName = $szName;
        return $this; // We now return $this (the Person)
    }
    
    public function setAge($iAge)
    {
        $this->m_iAge = $iAge;
        return $this; // Again, return our Person
    }
    
    public function introduce()
    {
          return  'Hello my name is '.$this->m_szName.' and I am '.$this->m_iAge.' years old.';
    }
}  

$smarty = new Smarty();
$smarty->force_compile = false;
$smarty->caching = false;
$smarty->caching_lifetime = 10;

$smarty->assign('foo','<bar>');

$person = new Person;

$smarty->assign('person',$person);

$smarty->assign('array',array('a'=>array('aa'=>'This is a long string'),'b'=>2));

$smarty->display('php:index_view.php');

?>
