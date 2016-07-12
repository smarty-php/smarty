<?php
/**
 * Smarty PHPunit tests deault plugin handler
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for plugin handler tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class DefaultPluginHandlerTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->setForceCompile(true);
        $this->smarty->disableSecurity();
        $this->smarty->registerDefaultPluginHandler('my_plugin_handler');
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    public function testDefaultFunctionScript()
    {
        $this->assertEquals("scriptfunction foo bar", $this->smarty->fetch('test_default_function_script.tpl'));
    }

    public function testDefaultFunctionScriptNotCachable1()
    {
        $this->smarty->assign('foo', 'foo');
        $this->smarty->caching = 1;
        $this->assertEquals("scriptfunction foo", $this->smarty->fetch('test_default_function_script_notcachable.tpl'));
    }

    public function testDefaultFunctionScriptNotCachable2()
    {
        $this->smarty->assign('foo', 'bar');
        $this->smarty->caching = 1;
        $this->assertEquals("scriptfunction bar", $this->smarty->fetch('test_default_function_script_notcachable.tpl'));
    }

    public function testDefaultFunctionLocal()
    {
        $this->assertEquals("localfunction foo bar", $this->smarty->fetch('test_default_function_local.tpl'));
    }

    public function testDefaultCompilerFunctionScript()
    {
        $this->assertEquals("echo 'scriptcompilerfunction '.'foo bar';", $this->smarty->fetch('test_default_compiler_function_script.tpl'));
    }
    public function testDefaultCompilerObject()
    {
        $this->assertEquals('Public World', $this->smarty->fetch('test_default_compiler_object.tpl'));
    }

    public function testDefaultBlockScript()
    {
        $this->assertEquals("scriptblock foo bar", $this->smarty->fetch('test_default_block_script.tpl'));
    }

    public function testDefaultModifierScript()
    {
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals("scriptmodifier default bar", $this->smarty->fetch('test_default_modifier_script.tpl'));
    }

    public function testDefaultModifier()
    {
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals("localmodifier bar", $this->smarty->fetch('test_default_modifier.tpl'));
    }

    public function testDefaultModifierStaticClassMethodCaching1()
    {
        $this->smarty->assign('foo', 'bar');
        $this->smarty->caching = 1;
        $this->assertEquals("staticmodifier bar", $this->smarty->fetch('test_default_static_modifier.tpl'));
    }

    public function testDefaultModifierStaticClassMethodCaching2()
    {
        $this->smarty->assign('foo', 'bar');
        $this->smarty->caching = 1;
        $this->assertEquals("staticmodifier bar", $this->smarty->fetch('test_default_static_modifier.tpl'));
    }
}

function my_plugin_handler($tag, $type, $template, &$callback, &$script, &$cachable)
{
    switch ($type) {
        case Smarty::PLUGIN_FUNCTION:
            switch ($tag) {
                case 'scriptfunction':
                    $script = './scripts/script_function_tag.php';
                    $callback = 'default_script_function_tag';

                    return true;
                case 'scriptfunctionnotcachable':
                    $script = './scripts/script_function_tag.php';
                    $callback = 'default_script_function_tag';
                    $cachable = false;

                    return true;
                case 'localfunction':
                    $callback = 'default_local_function_tag';

                    return true;
                default:
                    return false;
            }
        case Smarty::PLUGIN_COMPILER:
            switch ($tag) {
                case 'scriptcompilerfunction':
                    $script = './scripts/script_compiler_function_tag.php';
                    $callback = 'default_script_compiler_function_tag';
                    return true;
                case 'compilerobject':
                     $callback = array(new CompilerDefaultPluginClass, 'compile');
                    return true;
                default:
                    return false;
            }
        case Smarty::PLUGIN_BLOCK:
            switch ($tag) {
                case 'scriptblock':
                    $script = './scripts/script_block_tag.php';
                    $callback = 'default_script_block_tag';

                    return true;
                default:
                    return false;
            }
        case Smarty::PLUGIN_MODIFIER:
            switch ($tag) {
                case 'scriptmodifier':
                    $script = './scripts/script_modifier.php';
                    $callback = 'default_script_modifier';

                    return true;
                case 'mydefaultmodifier':
                    $callback = 'default_local_modifier';

                    return true;
                case 'mydefaultstaticmodifier':
                    $script = './scripts/script_default_static_modifier.php';
                    $callback = array('DefModifier', 'default_static_modifier');

                    return true;
                default:
                    return false;
            }
        default:
            return false;
    }
}

function default_local_function_tag($params, $template)
{
    return 'localfunction ' . $params['value'];
}

function default_local_modifier($input)
{
    return 'localmodifier ' . $input;
}
class CompilerDefaultPluginClass
{
    static function statCompile ($params, $compiler) {
        return '<?php echo \'Static World\';?>';
    }
    public function compile ($params, $compiler) {
        return '<?php echo \'Public World\';?>';
    }
}