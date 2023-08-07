<?php
/**
 * Smarty PHPunit tests compilation of registered object functions
 *

 * @author  Uwe Tews
 */

/**
 * class for registered object function tests
 *
 * 
 * 
 * 
 */
class CompileRegisteredObjectFunctionTest extends PHPUnit_Smarty
{
    /**
     * @var RegObject
     */
    private $object;

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);

        $this->smarty->setForceCompile(true);
        $this->smarty->disableSecurity();
        $this->object = new RegObject;
        $this->smarty->registerObject('objecttest', $this->object, 'myhello', true, 'myblock');
        $this->smarty->registerObject('objectprop', $this->object);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test resgistered object as function
     */
    public function testRegisteredObjectFunction()
    {
        $tpl = $this->smarty->createTemplate('eval:{objecttest->myhello}');
        $this->assertEquals('hello world', $this->smarty->fetch($tpl));
    }

    /**
     * test resgistered object as function with modifier
     */
    public function testRegisteredObjectFunctionModifier()
    {
        $tpl = $this->smarty->createTemplate('eval:{objecttest->myhello|truncate:6}');
        $this->assertEquals('hel...', $this->smarty->fetch($tpl));
    }

    /**
     * test resgistered object as block function
     */
    public function testRegisteredObjectBlockFunction()
    {
        $tpl = $this->smarty->createTemplate('eval:{objecttest->myblock}hello world{/objecttest->myblock}');
        $this->assertEquals('block test', $this->smarty->fetch($tpl));
    }

    public function testRegisteredObjectBlockFunctionModifier1()
    {
        $tpl = $this->smarty->createTemplate('eval:{objecttest->myblock}hello world{/objecttest->myblock|upper}');
        $this->assertEquals(strtoupper('block test'), $this->smarty->fetch($tpl));
    }

    public function testRegisteredObjectBlockFunctionModifier2()
    {
        $tpl = $this->smarty->createTemplate('eval:{objecttest->myblock}hello world{/objecttest->myblock|default:""|upper}');
        $this->assertEquals(strtoupper('block test'), $this->smarty->fetch($tpl));
    }
    // TODO

    /**
    public function testRegisteredObjectProperty()
    {
        $tpl = $this->smarty->createTemplate('eval:{objectprop->prop}');
        $this->assertEquals('hello world', $this->smarty->fetch($tpl));
    }

    public function testRegisteredObjectPropertyAssign()
    {
        $tpl = $this->smarty->createTemplate('eval:{objectprop->prop assign="foo"}{$foo}');
        $this->assertEquals('hello world', $this->smarty->fetch($tpl));
    }
     */
}

Class RegObject
{
    public $prop = 'hello world';

    public function myhello($params)
    {
        return 'hello world';
    }

    public function myblock($params, $content, &$smarty_tpl, &$repeat)
    {
        if (isset($content)) {
            $output = str_replace('hello world', 'block test', $content);

            return $output;
        }
    }
}
