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
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
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
        $this->smarty->disableSecurity();
	    $this->smarty->registerPlugin('modifier', 'pass', 'pass');
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
        $this->smarty->disableSecurity();
	    $this->smarty->registerPlugin('modifier', 'pass', 'pass');
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
	    $this->smarty->registerPlugin('modifier', 'pass', 'pass');
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
	    $this->smarty->registerPlugin('modifier', 'intval', 'intval');
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

	/**
	 * Tests various PHP functions (deprecated)
	 * @dataProvider        dataVariousPHPFunctions
	 */
	public function testVariousPHPFunctions($strTemplate, $value, $expected) {
		$this->smarty->disableSecurity();
		$this->cleanDirs();
		$this->smarty->assign('value', $value);
		$this->assertEquals($expected, $this->smarty->fetch('string:' . $strTemplate));
	}

	/**
	 * Data provider for testIsset3
	 */
	public function dataVariousPHPFunctions()
	{
		return array(
			array('{$a = count($value)}{$a}', array(1,2,3), '3'),
			array('{$a = in_array("b", $value)}{$a}', array(1,'b',3), true),
			array('{$a = strlen(uniqid())}{$a}', '', 13),
			array('{$a = date("Y", $value)}{$a}', strtotime("01-01-2030"), 2030),
			array('{$a = PhpFunctionTest::sayHi($value)}{$a}', 'mario', 'hi mario'),
			array('{$a = pass($value)}{$a}', 'mario', 'mario'),
			array('{$a = 1}{$b = Closure::fromCallable($value)}{$a}', 'strlen', 1),
		);
	}

	public static function sayHi($value) {
		return 'hi ' . $value;
	}

	/**
	 * Tests that each function that will not be supported in Smarty 5 does throw an E_USER_DEPRECATED notice.
	 * @dataProvider        dataDeprecationNoticesForSmarty5
	 * @deprecated
	 */
	public function testDeprecationNoticesForSmarty5($strTemplateSource, $expected = '', $shouldTriggerDeprecationNotice = false) {

		// this overrides the error reporting level set in \PHPUnit_Smarty::setUpSmarty
		$previousErrorReporting = $this->smarty->error_reporting;
		$this->smarty->setErrorReporting($previousErrorReporting | E_USER_DEPRECATED);

		$this->smarty->assign('a', 'a');
		$this->smarty->assign('ar', [1,2]);
		$this->smarty->assign('f', 3.14);

		$errorMessage = '';
		$output = '';

		try {
			$output = $this->smarty->fetch('string:' . $strTemplateSource);
		} catch (Exception $e) {
			$errorMessage = $e->getMessage();
		}

		if ($shouldTriggerDeprecationNotice) {
			$this->assertStringContainsString('Using unregistered function', $errorMessage);
		} else {
			$this->assertEquals($expected, $output);
			$this->assertEquals('', $errorMessage);
		}

		$this->smarty->setErrorReporting($previousErrorReporting);
	}

	public function dataDeprecationNoticesForSmarty5()
	{

		return [

			['{if empty($a)}{else}b{/if}', 'b', false],
			['{json_encode($a)}', '"a"', false],
			['{nl2br($a)}', 'a', false],
			['{$a|nl2br}', 'a', false],
			['{round($f, 1)}', '3.1', false],
			['{$f|round}', '3', false],
			['{str_repeat($a, 2)}', 'aa', false],
			['{$a|str_repeat:3}', 'aaa', false],
			['{$a|strip_tags}', 'a', false],
			['{$a|strlen}', '1', false],
			['{$a|substr:-1}', 'a', false],
			['{$f|substr:-1}', '4', false],
			['{$ar|count}', '2', false],
			['{count($ar)}', '2', false],
			['{foreach "."|explode:$f as $n}{$n}{/foreach}', '314', false],
			['{"-"|implode:$ar}', '1-2', false],
			['{"-"|join:$ar}', '1-2', false],
			['{$f|wordwrap:2:"k":true}', "3.k14", false],
			['{$f|number_format:1:","}', "3,1", false],
			['{if in_array(1, $ar)}yes{/if}', "yes", false],
			['{if is_array($ar)}yes{/if}', "yes", false],
			['{if time() gt 0}yes{/if}', "yes", false],

			['{if array_chunk($ar, 2)}x{else}y{/if}', '', true],
			['{$a|addslashes}', '', true],
			['{$a|sha1}', '', true],
			['{$a|get_parent_class}', '', true],
		];
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
