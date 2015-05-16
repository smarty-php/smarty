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
        $this->smarty->setCompileId($this->getName());
        $this->smartyBC->setCompileId($this->getName());
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

/**
 * test <?php...\> tag
 * PHP_REMOVE
 */
public function testPHP_REMOVE_php()
{
    $this->smarty->setPhpHandling(Smarty::PHP_REMOVE);
    $content = $this->smarty->fetch("string:a<?php echo 'hello world'; ?>e");
    $this->assertEquals("a echo 'hello world'; e", $content, 'remove <?php ?>');
}

    /**
     * test <%...%> tag
     * PHP_REMOVE
     */
    public function testPHP_REMOVE_asp()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_REMOVE);
        $content = $this->smarty->fetch("string:a<% echo 'hello world';%>e");
        $this->assertEquals("a echo 'hello world';e", $content, 'remove <% %>');
    }
    /**
     * test <script language='php'>...</script> tag
     * PHP_REMOVE
     */
    public function testPHP_REMOVE_script()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_REMOVE);
        $content = $this->smarty->fetch("string:a<script language='php'> echo 'hello world';</script>e");
        $this->assertEquals("a echo 'hello world';e", $content, "remove <script language='php'>");
    }
    /**
     * test <?php...\> tag
     * PHP_PASSTHRU
     */
    public function testPHP_PASSTHRU_php()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_PASSTHRU);
        $content = $this->smarty->fetch("string:pa<?php echo 'hello world'; ?>pe");
        $this->assertEquals("pa<?php echo 'hello world'; ?>pe", $content, 'passthru <?php ?>');
    }
    /**
     * test <%...%> tag
     * PHP_PASSTHRU
     */
    public function testPHP_PASSTHRU_asp()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_PASSTHRU);
        $content = $this->smarty->fetch("string:pa<% echo 'hello world';%>pe");
        $this->assertEquals("pa<% echo 'hello world';%>pe", $content, 'passthru <% %>');
    }
    /**
     * test <script language='php'>...</script> tag
     * PHP_PASSTHRU
     */
    public function testPHP_PASSTHRU_script()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_PASSTHRU);
        $content = $this->smarty->fetch("string:pa<script language='php'> echo 'hello world';</script>pe");
        $this->assertEquals("pa<script language='php'> echo 'hello world';</script>pe", $content, "passthru <script language='php'>");
    }
    /**
     * test <?php...\> tag
     * PHP_QUOTE
     */
    public function testPHP_QUOTE_php()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_QUOTE);
        $content = $this->smarty->fetch("string:qa<?php echo 'hello world';\necho ' multiline'; ?>qe");
        $this->assertEquals("qa&lt;?php echo 'hello world';\necho ' multiline'; ?&gt;qe", $content, 'qoute <?php ?>');
    }
    /**
     * test <%...%> tag
     * PHP_QUOTE
     */
    public function testPHP_QUOTE_asp()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_QUOTE);
        $content = $this->smarty->fetch("string:qa<% echo 'hello world';%>qe");
        $this->assertEquals("qa&lt;% echo 'hello world';%&gt;qe", $content, 'qoute <% %>');
    }
    /**
     * test <script language='php'>...</script> tag
     * PHP_QUOTE
     */
    public function testPHP_QUOTE_script()
    {
        $this->smarty->setPhpHandling(Smarty::PHP_QUOTE);
        $content = $this->smarty->fetch("string:qa<script language='php'> echo 'hello world';</script>qe");
        $this->assertEquals("qa&lt;script language=&#039;php&#039;&gt; echo 'hello world';&lt;/script&gt;qe", $content, "quote <script language='php'>");
    }
    /**
     * test <?php...\> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW_php()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <?php echo 'hello world'; ?> ae");
    }
    /**
     * test <%...%> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW_asp()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <% echo 'hello world';\n echo ' multiline';%> ae");
        if (ini_get('asp_tags')) {
            $this->assertEquals('aa hello world multiline ae', $content, 'allow <% %>');
        } else {
            $this->assertEquals("aa <% echo 'hello world';\n echo ' multiline';%> ae", $content, 'allow asp disabled <% %>');
        }
    }
    /**
     * test <script language='php'>...</script> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW_script()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <script language='php'> echo 'hello world';\n echo ' multiline';</script> ae");
        $this->assertEquals('aa hello world multiline ae', $content, "allow <script language='php'>");
    }
    /**
     * test <?php...\> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW_php2()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <?php echo '<?php';\necho ' ?>'; ?> ae");
        $this->assertEquals('aa <?php ?> ae', $content);
    }
    /**
     * test <?php...\> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW_php3()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <?php echo '?>'; ?> ae");
        $this->assertEquals('aa ?> ae', $content);
    }
    /**
     * test <?php...\> tag
     * PHP_ALLOW
     */
    public function testPHP_ALLOW_php4()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <?php /* ?> */ echo '?>'; ?> ae");
        $this->assertEquals('aa ?> ae', $content);
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
     * test unmatched <?php
     *
     */
    public function testUnmatched_php()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch('string:aa <?php ee');
        $this->assertEquals('aa <?php ee', $content);
    }
    /**
     * test unmatched ?>
     *
     */
    public function testUnmatched_php_close()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch('string:aa ?> ee');
        $this->assertEquals('aa ?> ee', $content);
    }
    /**
     * test unmatched <%
     *
     */
    public function testUnmatched_asp()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch('string:aa <% ee');
        $this->assertEquals('aa <% ee', $content);
    }
    /**
     * test unmatched %>
     *
     */
    public function testUnmatched_asp_close()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch('string:aa %> ee');
        $this->assertEquals('aa %> ee', $content);
    }
    /**
     * test unmatched <script language='php'>
     *
     */
    public function testUnmatched_script()
    {
        $this->smartyBC->setPhpHandling(Smarty::PHP_ALLOW);
        $content = $this->smartyBC->fetch("string:aa <script language='php'> echo 'hello world'; ae");
        $this->assertEquals("aa <script language='php'> echo 'hello world'; ae", $content);
    }
    /**
     * test {php}{/php} tag
     * PHP_ALLOW
     */
    public function testPHP_Tag()
    {
        $content = $this->smartyBC->fetch("string:aa {php} echo 'hallo'; {/php} ae");
        $this->assertEquals('aa hallo ae', $content);
    }
    /**
     * test {php nocache}{/php} tag
     * PHP_ALLOW
     */
    public function testPHP_Tag_Nocache()
    {
        $this->smartyBC->caching = 1;
        $content = $this->smartyBC->fetch("string:aa {php nocache} echo 'hallo'; {/php} ae");
        $this->assertEquals('aa hallo ae', $content);
    }
    /**
     * test {php no cache}illegal option
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage illegal value of option flag "no cache"
     *
     */
    public function testPHP_Tag_IllegalOption()
    {
        $content = $this->smartyBC->fetch("string:aa {php no cache} echo 'hallo'; {/php} ae");
    }

    /**
     * test { php}{/php} tag
     * PHP_Tag Literal
     */
    public function testPHP_Tag_Literal()
    {
        $content = $this->smartyBC->fetch("string:aa { php} echo 'hallo'; {/php} ae");
        $this->assertEquals('aa { php} echo \'hallo\'; {/php} ae', $content);
    }
    /**
     * test unmatched {php} tag
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Missing {/php} closing tag
     *
     */
    public function testPHP_Tag_unmatch()
    {
        $content = $this->smartyBC->fetch("string:aa {php} echo 'hallo';  ae");
    }
    /**
     * test unmatched {/php} tag
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Missing {php} open tag
     *
     */
    public function testPHP_TagOpen_unmatch()
    {
        $content = $this->smartyBC->fetch("string:aa {/php}  ae");
    }
}
