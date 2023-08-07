<?php
/**
 * Smarty PHPunit tests of filter
 *

 * @author  Uwe Tews
 */

/**
 * class for filter tests
 *
 * 
 * @preserveGlobalState    disabled
 * 
 */
class FilterTest extends PHPUnit_Smarty
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
	 * test loaded filter
	 */
	public function testNoOutputFilter()
	{
		$tpl = $this->smarty->createTemplate('string:{"    <br>hello world"}');
		$this->assertEquals("    <br>hello world", $this->smarty->fetch($tpl));
	}

    /**
     * test loaded filter
     */
    public function testLoadedOutputFilter()
    {
        $this->smarty->loadFilter(\Smarty\Smarty::FILTER_OUTPUT, 'trimwhitespace');
        $tpl = $this->smarty->createTemplate('string:{"    <br>hello world"}');
        $this->assertEquals("<br>hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test registered output filter
     */
    public function testRegisteredOutputFilter()
    {
        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter');
        $tpl = $this->smarty->createTemplate('eval:{"hello   world"}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

	/**
	 * test unregister output filter
	 */
	public function testUnRegisterOutputFilter()
	{
		$this->smarty->registerFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter');
		$this->smarty->unRegisterFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter');
		$tpl = $this->smarty->createTemplate('eval:{"hello   world"}');
		$this->assertEquals("hello   world", $this->smarty->fetch($tpl));
	}

    /**
     * test registered output filter not cached
     */
    public function testRegisteredOutputFilter_001()
    {
        $this->smarty->assign('foo', 1);
        $this->smarty->assign('bar', 2);
        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter2');
        $this->assertEquals('1 filter 2', $this->smarty->fetch('output_001.tpl'));
    }

    /**
     * test registered output filter not cached 2"
     */
    public function testRegisteredOutputFilter_001_2()
    {
        $this->smarty->assign('foo', 3);
        $this->smarty->assign('bar', 4);
        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter2');
        $this->assertEquals('3 filter 4', $this->smarty->fetch('output_001.tpl'));
    }

    /**
     * test registered output filter cached 1"
     */
    public function testRegisteredOutputFilter_001_3()
    {
        $this->smarty->setCaching(true);
        $this->smarty->assign('foo', 5);
        $this->smarty->assign('bar', 6);
        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter2');
        $this->assertEquals('5 filter 6', $this->smarty->fetch('output_001.tpl'));

    }

    /**
     * test registered output filter cached 1"
     */
    public function testRegisteredOutputFilter_001_4()
    {
        $this->smarty->setCaching(true);
        $this->smarty->assign('foo', 7);
        $this->smarty->assign('bar', 8);
        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter2');
        $this->assertEquals('5 filter 6', $this->smarty->fetch('output_001.tpl'));
    }

    /**
     * test registered output filter cached nocache"
     */
    public function testRegisteredOutputFilter_002_1()
    {
        $this->smarty->setCaching(true);
        $this->smarty->assign('foo', 10);
        $this->smarty->assign('bar', 11);
        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter2');
        $this->assertEquals('10 filter 11', $this->smarty->fetch('output_002.tpl'));
    }

    /**
     * test registered output filter cached nocache"
     */
    public function testRegisteredOutputFilter_002_2()
    {
        $this->smarty->setCaching(true);
        $this->smarty->assign('foo', 12);
        $this->smarty->assign('bar', 13);
        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_OUTPUT, 'myoutputfilter2');
        $this->assertEquals('12 filter 13', $this->smarty->fetch('output_002.tpl'));
    }

    public function testRegisteredOutputFilterWrapper()
    {
        $this->smarty->registerFilter('output', 'myoutputfilter');
        $tpl = $this->smarty->createTemplate('eval:{"hello   world"}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test registered pre filter
     */
    public function testRegisteredPreFilter()
    {
        function myprefilter($input)
        {
            return '{$foo}' . $input;
        }

        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_PRE, 'myprefilter');
        $tpl = $this->smarty->createTemplate('eval:{" hello world"}');
        $tpl->assign('foo', 'bar');
        $this->assertEquals("bar hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test registered pre filter closure
     */

    public function testRegisteredPreFilterClosure()
    {
       include 'FilterClosure.php';
    }

    /**
     * test registered pre filter class
     */
    public function testRegisteredPreFilterClass()
    {
        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_PRE, array('myprefilterclass', 'myprefilter'));
        $tpl = $this->smarty->createTemplate('eval:{" hello world"}');
        $tpl->assign('foo', 'bar');
        $this->assertEquals("bar hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test registered post filter
     */
    public function testRegisteredPostFilter()
    {
        function mypostfilter($input)
        {
            return '{$foo}' . $input;
        }

        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_POST, 'mypostfilter');
        $tpl = $this->smarty->createTemplate('eval:{" hello world"}');
        $tpl->assign('foo', 'bar');
        $this->assertEquals('{$foo} hello world', $this->smarty->fetch($tpl));
    }

    /**
     * test variable filter
     */
    public function testLoadedVariableFilter()
    {
        $this->smarty->loadFilter(\Smarty\Smarty::FILTER_VARIABLE, "escape");
        $tpl = $this->smarty->createTemplate('eval:{$foo}');
        $tpl->assign('foo', '<?php ?>');
        $this->assertEquals('&lt;?php ?&gt;', $this->smarty->fetch($tpl));
    }

    /**
     * test registered variable filter
     */
    public function testRegisteredVariableFilter2()
    {
        $var = new VarFilter();

        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_VARIABLE, array($var, 'my_filter'));
        $tpl = $this->smarty->createTemplate('string:{$foo}');
        $tpl->assign('foo', 'bar');
        $this->assertEquals('var{$foo}bar', $this->smarty->fetch($tpl));
    }
}

Class VarFilter
{
    function my_filter($input)
    {
        return 'var{$foo}' . $input;
    }
}

function myoutputfilter($input)
{
    return str_replace('   ', ' ', $input);
}

function myoutputfilter2($input, $tpl)
{
    return $input . ' filter ' . $tpl->getValue('bar');
}

class myprefilterclass
{
    static function myprefilter($input)
    {
        return '{$foo}' . $input;
    }
}
