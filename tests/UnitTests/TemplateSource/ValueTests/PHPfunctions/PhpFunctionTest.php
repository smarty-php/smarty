<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 *
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PhpFunctionTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test PHP empty() on variables true
     */
    public function testEmpty1()
    {
        $this->smarty->disableSecurity();
        $this->smarty->assign('var', array(null,
                                           false,
                                           (int) 0,
                                           (float) 0.0,
                                           '',
                                           array()));
        $expected = ' true , true , true , true , true , true , true , true ';
        $this->assertEquals($expected, $this->smarty->fetch('string:{strip}{if empty($var[0])} true {else} false {/IF}
        ,{if empty($var[1])} true {else} false {/IF}
        ,{if empty($var[2])} true {else} false {/IF}
        ,{if empty($var[3])} true {else} false {/IF}
        ,{if empty($var[4])} true {else} false {/IF}
        ,{if empty($var[5])} true {else} false {/IF}
        ,{if empty($var[6])} true {else} false {/IF}
        ,{if empty($varr)} true {else} false {/IF}
        '));
    }

    /**
     * test PHP empty() on function result true
     */
    public function testEmpty2()
    {
        if (version_compare(phpversion(), '5.5', '<')) {
            $this->markTestSkipped('runs only on PHP >= 5.5');
        }

        $this->smarty->disableSecurity();
        $this->smarty->assign('var', array(null,
                                           false,
                                           (int) 0,
                                           (float) 0.0,
                                           '',
                                           array()));
        $expected = ' true , true , true , true , true , true ';
        $this->assertEquals($expected, $this->smarty->fetch('string:{strip}{if empty(pass($var[0]))} true {else} false {/IF}
        ,{if empty(pass($var[1]))} true {else} false {/IF}
        ,{if empty(pass($var[2]))} true {else} false {/IF}
        ,{if empty(pass($var[3]))} true {else} false {/IF}
        ,{if empty(pass($var[4]))} true {else} false {/IF}
        ,{if empty(pass($var[5]))} true {else} false {/IF}
        '));
    }
    /**
     * test PHP empty() on function result false
     */
    public function testEmpty3()
    {
        if (version_compare(phpversion(), '5.5', '<')) {
            $this->markTestSkipped('runs only on PHP >= 5.5');
        }
        $this->smarty->disableSecurity();
        $this->smarty->assign('var', array(true,
                                           (int) 1,
                                           (float) 0.1,
                                           ' ',
                                           array(1)));
        $expected = ' false , false , false , false , false ';
        $this->assertEquals($expected, $this->smarty->fetch('string:{strip}{if empty(pass($var[0]))} true {else} false {/IF}
        ,{if empty(pass($var[1]))} true {else} false {/IF}
        ,{if empty(pass($var[2]))} true {else} false {/IF}
        ,{if empty(pass($var[3]))} true {else} false {/IF}
        ,{if empty(pass($var[4]))} true {else} false {/IF}
        '));
    }
    /**
     * test PHP empty() on object
     */
    public function testEmpty4()
    {
        if (version_compare(phpversion(), '5.5', '<')) {
        $this->markTestSkipped('runs only on PHP >= 5.5');
        }

        $this->smarty->disableSecurity();
        $this->smarty->assign('var', new TestIsset());
        $expected = ' true , false , false , true , true , true , false ';
        $this->assertEquals($expected, $this->smarty->fetch('string:{strip}{if empty($var->isNull)} true {else} false {/IF}
        ,{if empty($var->isSet)} true {else} false {/IF}
        ,{if empty($var->arr[\'isSet\'])} true {else} false {/IF}
        ,{if empty($var->arr[\'isNull\'])} true {else} false {/IF}
        ,{if empty($var->arr[\'foo\'])} true {else} false {/IF}
        ,{if empty($var->pass(null))} true {else} false {/IF}
        ,{if empty($var->pass(1))} true {else} false {/IF}
        '));
    }
    /**
     * test PHP isset() on variables and functions
     */
    public function testIsset1()
    {
        $this->smarty->disableSecurity();
        $this->smarty->assign('isNull', null);
        $this->smarty->assign('isSet', 1);
        $this->smarty->assign('arr', array('isNull' => null, 'isSet' => 1));
        $expected = ' false , true , false , true , false , false , false , true ';
        $this->assertEquals($expected, $this->smarty->fetch('string:{strip}{if isset($isNull)} true {else} false {/IF}
        ,{if isset($isSet)} true {else} false {/IF}
        ,{if isset($foo)} true {else} false {/IF}
        ,{if isset($arr[\'isSet\'])} true {else} false {/IF}
        ,{if isset($arr[\'isNull\'])} true {else} false {/IF}
        ,{if isset($arr[\'foo\'])} true {else} false {/IF}
        ,{if isset(pass(null))} true {else} false {/IF}
        ,{if isset(pass(1))} true {else} false {/IF}
        '));
    }
    /**
     * test PHP isset() on object
     */
    public function testIsset2()
    {
        $this->smarty->disableSecurity();
        $this->smarty->assign('var', new TestIsset());
        $expected = ' false , true , true , false , false , false , true ';
        $this->assertEquals($expected, $this->smarty->fetch('string:{strip}{if isset($var->isNull)} true {else} false {/IF}
        ,{if isset($var->isSet)} true {else} false {/IF}
        ,{if isset($var->arr[\'isSet\'])} true {else} false {/IF}
        ,{if isset($var->arr[\'isNull\'])} true {else} false {/IF}
        ,{if isset($var->arr[\'foo\'])} true {else} false {/IF}
        ,{if isset($var->pass(null))} true {else} false {/IF}
        ,{if isset($var->pass(1))} true {else} false {/IF}
        '));
    }

	/**
	 * test PHP isset() on (non-)variables
	 * @dataProvider        dataTestIsset3
	 * @param string $strTemplate template to test
	 * @param string $result expected result
	 */
	public function testIsset3($strTemplate, $result)
	{
		$this->smarty->disableSecurity();

		$this->smarty->assign('varobject', new TestIsset());
		$this->smarty->assign('vararray', $vararray = array(
			'keythatexists' => false,
			'keywitharray' => array(1 => 1),
			'keywithobject' => new TestIsset()
		));

		$this->smarty->assign('key', 'A');
		$this->smarty->assign('_varsimpleA', 1);
		$this->smarty->assign('varsimpleB', 0);
		$this->smarty->assign('varsimpleC', null);

		$this->assertEquals($result, $this->smarty->fetch('string:' . $strTemplate));
	}

	/**
	 * Data provider for testIsset3
	 */
	public function dataTestIsset3()
	{
		return array(
			array('{if isset($varobject->arr)}true{else}false{/if}', 'true'),
			array('{if isset($vararray["keywitharray"])}true{else}false{/if}', 'true'),
			array('{if isset($vararray["keythatexists"])}true{else}false{/if}', 'true'),
			array('{if isset($vararray["nonexistingkey"])}true{else}false{/if}', 'false'),
			array('{if isset($_GET["sscr6hr6cz34j6"])}true{else}false{/if}', 'false'),
			array('{if isset(count([\'hi\']))}true{else}false{/if}', 'true'),
			array('{if isset($vararray[\'keywitharray\'][intval(\'1\')])}true{else}false{/if}', 'true'),
			array('{if isset($vararray[\'keywithobject\']->arr[\'isSet\'])}true{else}false{/if}', 'true'),
			array('{if isset($vararray[\'keywithobject\']->arr[\'isNull\'])}true{else}false{/if}', 'false'),
			array('{if isset($varobject->arr[\'isSet\'])}true{else}false{/if}', 'true'),
			array('{if isset($varobject->arr[\'isNull\'])}true{else}false{/if}', 'false'),
			array('{if isset($_varsimpleA)}true{else}false{/if}', 'true'),
			array('{if isset($varsimpleB)}true{else}false{/if}', 'true'),
			array('{if isset($varsimpleC)}true{else}false{/if}', 'false'),
			array('{if isset($_varsimpleA && varsimpleB)}true{else}false{/if}', 'true'),
			array('{if isset($_varsimpleA && varsimpleC)}true{else}false{/if}', 'true'),
			array('{if isset($_varsimple{$key})}true{else}false{/if}', 'true'),
		);
	}
}

/**
 * @param mixed $v
 *
 * @return mixed
 */
function pass($v) {
    return $v;
}

/**
 * Class TestIsset
 */
class TestIsset {
    public $isNull = null;
    public $isSet = 1;
    public $arr = array('isNull' => null, 'isSet' => 1);

    /**
     * @param mixed $v
     *
     * @return mixed
     */
    public function pass($v) {
        return $v;
    }
}
