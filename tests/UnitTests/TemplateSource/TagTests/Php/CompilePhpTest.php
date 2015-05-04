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
 * @backupStaticAt tributes enabled
 */
class CompilePhpTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
        $this->smartyBC->disableSecurity();
        $this->smarty->disableSecurity();
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

/**
 * test <?php...\> tag
 * PHP_REMOVE
 */
public function testPHP_REMOVEphp()
{
    $this->smarty->setPhpHandling(Smarty::PHP_REMOVE);
    $content = $this->smarty->fetch("string:a<?php echo 'hello world'; ?>e");
    $this->assertEquals("a echo 'hello world'; e", $content, 'remove <?php ?>');
}

    /**
     * test <%...%> tag
     * PHP_REMOVE
     */
    public function testPHP_REMOVEasp()
    {
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smarty->setPhpHandling(Smarty::PHP_REMOVE);
        $content = $this->smarty->fetch("string:a<% echo 'hello world';%>e");
        $this->assertEquals("a echo 'hello world';e", $content, 'remove <% %>');


    }
    /**
     * test <?php...\> tag
     * PHP_PASSTHRU
     */
    public function testPHP_PASSTHRUphp()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_PASSTHRU);
        $content = $this->smarty->fetch("string:pa<?php echo 'hello world'; ?>pe");
        $this->assertEquals("pa<?php echo 'hello world'; ?>pe", $content, 'passthru <?php ?>');
    }
    /**
     * test <%...%> tag
     * PHP_PASSTHRU
     */
    public function testPHP_PASSTHRUasp()
    {
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smarty->setPhpHandling(Smarty::PHP_PASSTHRU);
         $content = $this->smarty->fetch("string:pa<% echo 'hello world';%>pe");
        $this->assertEquals("pa<% echo 'hello world';%>pe", $content, 'passthru <% %>');
    }
    /**
     * test <?php...\> tag
     * PHP_QUOTE
     */
    public function testPHP_QUOTEphp()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_QUOTE);
        $content = $this->smarty->fetch("string:qa<?php echo 'hello world'; ?>qe");
        $this->assertEquals("qa&lt;?php echo 'hello world'; ?&gt;qe", $content, 'qoute <?php ?>');
    }
    /**
     * test <%...%> tag
     * PHP_QUOTE
     */
    public function testPHP_QUOTEasp()
    {
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smarty->setPhpHandling(Smarty::PHP_QUOTE);
        $content = $this->smarty->fetch("string:qa<% echo 'hello world';%>qe");
        $this->assertEquals("qa&lt;% echo 'hello world';%&gt;qe", $content, 'qoute <% %>');
    }
    /**
     * test <?php...\> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOWphp()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <?php echo 'hello world'; ?> ae");
    }
    /**
     * test <%...%> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOWasp()
    {
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <% echo 'hello world';%> ae");
        $this->assertEquals('aa hello world ae', $content, 'allow <% %>');
    }
    /**
     * test <?php...\> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW2()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <?php echo '<?php'; ?> ae");
        $this->assertEquals('aa <?php ae', $content);
    }
    /**
     * test <?php...\> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW3()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <?php echo '?>'; ?> ae");
        $this->assertEquals('aa ?> ae', $content);
    }

    /**
     * test {php}{/php} tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW5()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa {php} echo 'hallo'; {/php} ae");
        $this->assertEquals('aa hallo ae', $content);
    }
    /**
     * test {php}{/php} tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW6()
    {
        $this->smartyBC->caching = 1;
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa {php nocache} echo 'hallo'; {/php} ae");
        $this->assertEquals('aa hallo ae', $content);
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage PHP in template not allowed
     */

    public function testPHP_ALLOW_error()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smarty->fetch("string:aa <?php echo 'hallo'; ?> ae");
    }
    /**
     * test <?php...\> tag
     * default is PASSTHRU
     */
    public function testPhpTag()
    {
        $tpl = $this->smartyBC->createTemplate("eval:<?php echo 'hello world'; ?>");
        $content = $this->smartyBC->fetch($tpl);
        $this->assertEquals("<?php echo 'hello world'; ?>", $content);
    }

    // ALLOW
    public function testPhpTagAllow()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $this->smartyBC->disableSecurity();
        $tpl = $this->smartyBC->createTemplate("eval:<?php echo 'hello world'; ?>");
        $content = $this->smartyBC->fetch($tpl);
        $this->assertEquals('hello world', $content);
    }

    /**
     * test <?=...\> shorttag
     * default is PASSTHRU
     */
    public function testShortTag()
    {
        $this->smartyBC->assign('foo', 'bar');
        $content = $this->smartyBC->fetch('eval:<?=$foo?>');
        $this->assertEquals('<?=$foo?>', $content);
    }
    /**
     * PHP tag data provider
     */
    public function includeProvider()
    {
        return array(
            array(SMARTY::PHP_REMOVE, 'normal'),
            array(true, 'merged'),
        );
    }
}
