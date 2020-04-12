<?php
/**
 * Smarty PHPunit tests compilation of {php} and <?php...?> tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {php} and <?php...?> tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompilePhpTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->setUseSubDirs(true);
        $this->smartyBC->setUseSubDirs(true);
        $this->smartyBC->disableSecurity();
        $this->smarty->disableSecurity();
        $this->smarty->setCompileId($this->getName());
        $this->smartyBC->setCompileId($this->getName());
    }

    public function testInit()
    {
        $this->cleanDirs();
    }


    /**
     * Test
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     *
     */
    public function testPHP($phpHandling, $templateFile, $result, $testName)
    {
        $result = str_replace("\r", '', $result);
        $this->smartyBC->force_compile = true;
        $this->smartyBC->php_handling = $phpHandling;
        $this->smartyBC->compile_id = $testName.$this->smartyBC->php_handling ;
        $tpl = $this->smartyBC->createTemplate($templateFile);
        if ($phpHandling == Smarty::PHP_PASSTHRU || $phpHandling == Smarty::PHP_QUOTE) {
            $result = str_replace("\r", '', $tpl->source->getContent());
        }
        if ($phpHandling == Smarty::PHP_QUOTE) {
            $result = preg_replace_callback('#(<\?(?:php|=)?)|(<%)|(<script\s+language\s*=\s*["\']?\s*php\s*["\']?\s*>)|(\?>)|(%>)|(<\/script>)#i', array($this, 'quote'), $result);
        }
        $content = $tpl->fetch();
        $this->assertEquals($result, $content, $testName);
    }   /**
 * Test
 * @runInSeparateProcess
 * @preserveGlobalState disabled
 * @dataProvider data
 *
 */
    public function testPHPcaching($phpHandling, $templateFile, $result, $testName)
    {
        $result = str_replace("\r", '', $result);
        $this->smartyBC->php_handling = $phpHandling;
        $this->smartyBC->compile_id = $testName.$this->smartyBC->php_handling ;
        $this->smartyBC->caching = true;
        $this->smartyBC->force_cache = true;
        $tpl = $this->smartyBC->createTemplate($templateFile);
        if ($phpHandling == Smarty::PHP_PASSTHRU || $phpHandling == Smarty::PHP_QUOTE) {
            $result = str_replace("\r", '', $tpl->source->getContent());
        }
        if ($phpHandling == Smarty::PHP_QUOTE) {
            $result = preg_replace_callback('#(<\?(?:php|=)?)|(<%)|(<script\s+language\s*=\s*["\']?\s*php\s*["\']?\s*>)|(\?>)|(%>)|(<\/script>)#i', array($this, 'quote'), $result);
        }
        $content = $tpl->fetch();
        $this->assertEquals($result, $content, $testName);
    }
    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage $smarty->php_handling PHP_ALLOW not allowed. Use SmartyBC to enable it
     */

    public function testPHP_ALLOW_error()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smarty->fetch("string:aa <?php echo 'hallo'; ?> ae");
    }

    /**
     * test {php nocache}{/php} tag
     */
    public function testPHP_Tag_Nocache1()
    {
        $this->smartyBC->caching = 1;
        $this->smartyBC->assign('foo', 'foo');
        $this->smartyBC->compile_id = 'Tag nocache' ;
        $content = $this->smartyBC->fetch('phptag_nocache.tpl');
        $this->assertEquals('-->foo<--', $content,'Tag nocache 1');
    }

    public function testPHP_Tag_Nocache2()
    {
        $this->smartyBC->caching = 1;
        $this->smartyBC->assign('foo', 'bar');
        $this->smartyBC->compile_id = 'Tag nocache' ;
        $content = $this->smartyBC->fetch('phptag_nocache.tpl');
        $this->assertEquals('-->bar<--', $content,'Tag nocache 2');
    }
    /**
     * test {php no cache}illegal option
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage illegal value of option flag 'no cache'
     *
     */
    public function testPHP_Tag_IllegalOption()
    {
        $content = $this->smartyBC->fetch("string:aa {php no cache} echo 'hallo'; {/php} ae");
    }

    /**
     * Test regression that plugin names starting with 'php' do work
     */
    public function testPHPfooPlugin()
    {
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $this->smartyBC->compile_id = 'Tag nocache' ;
        $this->assertEquals('phptest okay', $this->smarty->fetch('string:{phptest}'),'testPHPfooPlugin');
    }

    public function data()
    {
        $shortTag = ini_get('short_open_tag') == 1;
        $aspTag = ini_get('asp_tags') == 1;

        $data = array(
            /*
            * php_handling
            * template file
            * result
            * text
            */
            array(Smarty::PHP_REMOVE, 'php.tpl', '--><--', 'PHP_REMOVE, \'php.tpl\''),
            array(Smarty::PHP_PASSTHRU, 'php.tpl', '', 'PHP_PASSTHRU, \'php.tpl\''),
            array(Smarty::PHP_QUOTE, 'php.tpl', '', 'PHP_QUOTE, \'php.tpl\''),
            array(Smarty::PHP_ALLOW, 'php.tpl', '--> hello world <?php ?> <--', 'PHP_ALLOW, \'php.tpl\''),
            array(Smarty::PHP_REMOVE, 'php_line_comment.tpl', '--><--', 'PHP_REMOVE, \'php_line_comment.tpl\''),
            array(Smarty::PHP_PASSTHRU, 'php_line_comment.tpl', '', 'PHP_PASSTHRU, \'php_line_comment.tpl\''),
            array(Smarty::PHP_QUOTE, 'php_line_comment.tpl', '', 'PHP_QUOTE, \'php_line_comment.tpl\''),
            array(Smarty::PHP_ALLOW, 'php_line_comment.tpl', '--> hello world <?php ?> <--', 'PHP_ALLOW, \'php_line_comment.tpl\''),
            array(Smarty::PHP_REMOVE, 'php_block_comment.tpl', '--><--', 'PHP_REMOVE, \'php_block_comment.tpl\''),
            array(Smarty::PHP_PASSTHRU, 'php_block_comment.tpl', '', 'PHP_PASSTHRU, \'php_block_comment.tpl\''),
            array(Smarty::PHP_QUOTE, 'php_block_comment.tpl', '', 'PHP_QUOTE, \'php_block_comment.tpl\''),
            array(Smarty::PHP_ALLOW, 'php_block_comment.tpl', '--> hello world <?php ?> <--', 'PHP_ALLOW, \'php_block_comment.tpl\''),
            array(Smarty::PHP_REMOVE, 'php2.tpl', '--><--', 'PHP_REMOVE, \'php2.tpl\''),
            array(Smarty::PHP_PASSTHRU, 'php2.tpl', '', 'PHP_PASSTHRU, \'php2.tpl\''),
            array(Smarty::PHP_QUOTE, 'php2.tpl', '', 'PHP_QUOTE, \'php2.tpl\''),
            array(Smarty::PHP_ALLOW, 'php2.tpl', $shortTag || strpos(phpversion(), 'hhvm') !== false ? '--> hello world <?  ?> <--' : '--><? echo \' hello world \';
echo \'<?  \';
echo \'?> \';
?><--', 'PHP_ALLOW, \'php2.tpl\''),
            array(Smarty::PHP_REMOVE, 'asp.tpl', '--><--', 'PHP_REMOVE, \'asp.tpl\''),
            array(Smarty::PHP_PASSTHRU, 'asp.tpl', '', 'PHP_PASSTHRU, \'asp.tpl\''),
            array(Smarty::PHP_QUOTE, 'asp.tpl', '', 'PHP_QUOTE, \'asp.tpl\''),
            array(Smarty::PHP_ALLOW, 'asp.tpl', $aspTag ? '-->hello world <% %> <--' : '--><% echo \'hello world \';
echo \'<% \';
echo \'%> \';
%><--', 'PHP_ALLOW, \'asp.tpl\''),
            array(Smarty::PHP_REMOVE, 'script.tpl', '<br><br>', 'PHP_REMOVE, \'script.tpl\''),
            array(Smarty::PHP_PASSTHRU, 'script.tpl', '', 'PHP_PASSTHRU, \'script.tpl\''),
            array(Smarty::PHP_QUOTE, 'script.tpl', '', 'PHP_QUOTE, \'script.tpl\''),
            array(Smarty::PHP_ALLOW, 'phptag.tpl', '--> hello world {php} {/php} <--', 'PHP_ALLOW, \'phptag.tpl\''),
            array(Smarty::PHP_ALLOW, 'phptag_line_comment.tpl', '--> hello world {php} {/php} <--', 'PHP_ALLOW, \'phptag_line_comment.tpl\''),
            array(Smarty::PHP_ALLOW, 'phptag_block_comment.tpl', '--> hello world {php} {/php} <--', 'PHP_ALLOW, \'phptag_block_comment.tpl\''),
            array(Smarty::PHP_ALLOW, 'phptag_literal.tpl', '-->{ php} echo \' hello world \';
echo \'foo \';
echo \'bar \';
$foo = 3;
{ /php}<--', 'PHP_ALLOW, \'phptag_literal.tpl\''),
        );
        if (version_compare(phpversion(), '5.7.0', '<')) {
            $data[] = array(Smarty::PHP_ALLOW, 'script.tpl', '<br> This is a script <br> 5<br>', 'PHP_ALLOW, \'script.tpl\'');
        }
        return $data;
    }
    /*
 * Call back function for $php_handling = PHP_QUOTE
 *
 */
    private function quote($match)
    {
        return htmlspecialchars($match[0], ENT_QUOTES);
    }

}
