<?php
/**
 * Smarty PHPunit tests of filter
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for filter tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class FilterTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test autoload output filter
     */
    public function testAutoloadOutputFilter()
    {
        $this->smarty->autoload_filters['output'] = 'trimwhitespace';
        $tpl = $this->smarty->createTemplate('eval:{"    <br>hello world"}');
        $this->assertEquals("<br>hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test autoload variable filter
     */
    public function testAutoloadVariableFilter()
    {
        $this->smarty->autoload_filters['variable'] = 'htmlspecialchars';
        $tpl = $this->smarty->createTemplate('eval:{"<test>"}');
        $this->assertEquals("&lt;test&gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test loaded filter
     */
    public function testLoadedOutputFilter()
    {
        $this->smarty->loadFilter(Smarty::FILTER_OUTPUT, 'trimwhitespace');
        $tpl = $this->smarty->createTemplate('string:{"    <br>hello world"}');
        $this->assertEquals("<br>hello world", $this->smarty->fetch($tpl));
    }

    public function testLoadedOutputFilterWrapper()
    {
        $this->smartyBC->load_filter(Smarty::FILTER_OUTPUT, 'trimwhitespace');
        $tpl = $this->smartyBC->createTemplate('eval:{"    <br>hello world"}');
        $this->assertEquals("<br>hello world", $this->smartyBC->fetch($tpl));
    }

    /**
     * test registered output filter
     */
    public function testRegisteredOutputFilter()
    {
        $this->smarty->registerFilter(Smarty::FILTER_OUTPUT, 'myoutputfilter');
        $tpl = $this->smarty->createTemplate('eval:{"hello   world"}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    public function testRegisteredOutputFilterWrapper()
    {
        $this->smartyBC->register_outputfilter('myoutputfilter');
        $tpl = $this->smartyBC->createTemplate('eval:{"hello   world"}');
        $this->assertEquals("hello world", $this->smartyBC->fetch($tpl));
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

        $this->smarty->registerFilter(Smarty::FILTER_PRE, 'myprefilter');
        $tpl = $this->smarty->createTemplate('eval:{" hello world"}');
        $tpl->assign('foo', 'bar');
        $this->assertEquals("bar hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test registered pre filter closure
     */
    public function testRegisteredPreFilterClosure()
    {
        $this->smarty->registerFilter(Smarty::FILTER_PRE, function($input) {return '{$foo}' . $input;});
        $tpl = $this->smarty->createTemplate('eval:{" hello world"}');
        $tpl->assign('foo', 'buh');
        $this->assertEquals("buh hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test registered pre filter class
     */
    public function testRegisteredPreFilterClass()
    {
        $this->smarty->registerFilter(Smarty::FILTER_PRE, array('myprefilterclass', 'myprefilter'));
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

        $this->smarty->registerFilter(Smarty::FILTER_POST, 'mypostfilter');
        $tpl = $this->smarty->createTemplate('eval:{" hello world"}');
        $tpl->assign('foo', 'bar');
        $this->assertEquals('{$foo} hello world', $this->smarty->fetch($tpl));
    }

    /**
     * test variable filter
     */
    public function testLoadedVariableFilter()
    {
        $this->smarty->loadFilter("variable", "htmlspecialchars");
        $tpl = $this->smarty->createTemplate('eval:{$foo}');
        $tpl->assign('foo', '<?php ?>');
        $this->assertEquals('&lt;?php ?&gt;', $this->smarty->fetch($tpl));
    }

    /**
     * test registered post filter
     */
    public function testRegisteredVariableFilter2()
    {
        $var = new VarFilter();

        $this->smarty->registerFilter(Smarty::FILTER_VARIABLE, array($var, 'variablefilter'));
        $tpl = $this->smarty->createTemplate('string:{$foo}');
        $tpl->assign('foo', 'bar');
        $this->assertEquals('var{$foo}bar', $this->smarty->fetch($tpl));
    }
}

Class VarFilter
{
    function variablefilter($input, $smarty)
    {
        return 'var{$foo}' . $input;
    }
}

function myoutputfilter($input)
{
    return str_replace('   ', ' ', $input);
}

class myprefilterclass
{
    static function myprefilter($input)
    {
        return '{$foo}' . $input;
    }
}
