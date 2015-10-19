<?php
/**
 * Smarty PHPunit tests for security
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for security test
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class SecurityTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);

        $this->smarty->setForceCompile(true);
        $this->smarty->enableSecurity();
        $this->smartyBC->setForceCompile(true);
        $this->smartyBC->enableSecurity();
        $this->cleanDir($this->smarty->getCacheDir());
        $this->cleanDir($this->smarty->getCompileDir());
    }

    /**
     * test that security is loaded
     */
    public function testSecurityLoaded()
    {
        $this->assertTrue(is_object($this->smarty->security_policy));
    }

    /**
     * test trusted PHP function
     */
    public function testTrustedPHPFunction()
    {
        $this->assertEquals("5", $this->smarty->fetch('eval:{assign var=foo value=[1,2,3,4,5]}{count($foo)}'));
    }

    /**
     * test not trusted PHP function
     */
    public function testNotTrustedPHPFunction()
    {
        $this->smarty->security_policy->php_functions = array('null');
        try {
            $this->smarty->fetch('eval:{assign var=foo value=[1,2,3,4,5]}{count($foo)}');
        }
        catch (Exception $e) {
            $this->assertContains(htmlentities("PHP function 'count' not allowed by security setting"), $e->getMessage());

            return;
        }
        $this->fail('Exception for not trusted modifier has not been raised.');
    }

    /**
     * test not trusted PHP function at disabled security
     */
    public function testDisabledTrustedPHPFunction()
    {
        $this->smarty->security_policy->php_functions = array('null');
        $this->smarty->disableSecurity();
        $this->assertEquals("5", $this->smarty->fetch('eval:{assign var=foo value=[1,2,3,4,5]}{count($foo)}'));
    }

    /**
     * test trusted modifier
     */
    public function testTrustedModifier()
    {
        $this->assertEquals("5", $this->smarty->fetch('eval:{assign var=foo value=[1,2,3,4,5]}{$foo|@count}'));
    }

    /**
     * test not trusted modifier
     */
    public function testNotTrustedModifier()
    {
        $this->smarty->security_policy->php_modifiers = array('null');
        try {
            $this->smarty->fetch('eval:{assign var=foo value=[1,2,3,4,5]}{$foo|@count}');
        }
        catch (Exception $e) {
            $this->assertContains(htmlentities("modifier 'count' not allowed by security setting"), $e->getMessage());

            return;
        }
        $this->fail('Exception for not trusted modifier has not been raised.');
    }

    /**
     * test not trusted modifier at disabled security
     */
    public function testDisabledTrustedModifier()
    {
        $this->smarty->security_policy->php_modifiers = array('null');
        $this->smarty->disableSecurity();
        $this->assertEquals("5", $this->smarty->fetch('eval:{assign var=foo value=[1,2,3,4,5]}{$foo|@count}'));
    }

    /**
     * test allowed tags
     */
    public function testAllowedTags1()
    {
        $this->smarty->security_policy->allowed_tags = array('counter');
        $this->assertEquals("1", $this->smarty->fetch('eval:{counter start=1}'));
    }

    /**
     * test not allowed tag
     */
    public function testNotAllowedTags2()
    {
        $this->smarty->security_policy->allowed_tags = array('counter');
        try {
            $this->smarty->fetch('eval:{counter}{cycle values="1,2"}');
        }
        catch (Exception $e) {
            $this->assertContains(htmlentities("tag 'cycle' not allowed by security setting"), $e->getMessage());

            return;
        }
        $this->fail('Exception for not allowed tag has not been raised.');
    }

    /**
     * test disabled tag
     */
    public function testDisabledTags()
    {
        $this->smarty->security_policy->disabled_tags = array('cycle');
        try {
            $this->smarty->fetch('eval:{counter}{cycle values="1,2"}');
        }
        catch (Exception $e) {
            $this->assertContains(htmlentities("tag 'cycle' disabled by security setting"), $e->getMessage());

            return;
        }
        $this->fail('Exception for disabled tag has not been raised.');
    }

    /**
     * test allowed modifier
     */
    public function testAllowedModifier1()
    {
        error_reporting(E_ALL  & ~E_DEPRECATED | E_STRICT);
        $this->smarty->security_policy->allowed_modifiers = array('capitalize');
        $this->assertEquals("Hello World", $this->smarty->fetch('eval:{"hello world"|capitalize}'));
        error_reporting(E_ALL | E_STRICT);
    }

    public function testAllowedModifier2()
    {
        $this->smarty->security_policy->allowed_modifiers = array('upper');
        $this->assertEquals("HELLO WORLD", $this->smarty->fetch('eval:{"hello world"|upper}'));
    }

    /**
     * test not allowed modifier
     */
    public function testNotAllowedModifier()
    {
        $this->smarty->security_policy->allowed_modifiers = array('upper');
        try {
            $this->smarty->fetch('eval:{"hello"|upper}{"world"|lower}');
        }
        catch (Exception $e) {
            $this->assertContains(htmlentities("modifier 'lower' not allowed by security setting"), $e->getMessage());

            return;
        }
        $this->fail('Exception for not allowed tag has not been raised.');
    }

    /**
     * test disabled modifier
     */
    public function testDisabledModifier()
    {
        $this->smarty->security_policy->disabled_modifiers = array('lower');
        try {
            $this->smarty->fetch('eval:{"hello"|upper}{"world"|lower}');
        }
        catch (Exception $e) {
            $this->assertContains(htmlentities("modifier 'lower' disabled by security setting"), $e->getMessage());

            return;
        }
        $this->fail('Exception for disabled tag has not been raised.');
    }

    /**
     * test Smarty::PHP_QUOTE
     */
    public function testSmartyPhpQuote()
    {
        $this->smarty->security_policy->php_handling = Smarty::PHP_QUOTE;
        $this->assertEquals('&lt;?php echo "hello world"; ?&gt;', $this->smarty->fetch('eval:<?php echo "hello world"; ?>'));
    }

    public function testSmartyPhpQuoteAsp()
    {
        // NOTE: asp_tags cannot be changed by ini_set()
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smarty->security_policy->php_handling = Smarty::PHP_QUOTE;
        $this->assertEquals('&lt;% echo "hello world"; %&gt;', $this->smarty->fetch('eval:<% echo "hello world"; %>'));
    }

    /**
     * test Smarty::PHP_REMOVE
     */
    public function testSmartyPhpRemove()
    {
        $this->smarty->security_policy->php_handling = Smarty::PHP_REMOVE;
        $this->assertEquals('', $this->smarty->fetch('eval:<?php echo "hello world"; ?>'));
    }

    public function testSmartyPhpRemoveAsp()
    {
        // NOTE: asp_tags cannot be changed by ini_set()
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smarty->security_policy->php_handling = Smarty::PHP_REMOVE;
        $this->assertEquals('', $this->smarty->fetch('eval:<% echo "hello world"; %>'));
    }

    /**
     * test Smarty::PHP_ALLOW
     */
    public function testSmartyPhpAllow()
    {
        $this->smartyBC->security_policy->php_handling = Smarty::PHP_ALLOW;
        $this->assertEquals('hello world', $this->smartyBC->fetch('eval:<?php echo "hello world"; ?>'));
    }

    public function testSmartyPhpAllowAsp()
    {
        // NOTE: asp_tags cannot be changed by ini_set()
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smartyBC->security_policy->php_handling = Smarty::PHP_ALLOW;
        $this->assertEquals('hello world', $this->smartyBC->fetch('eval:<% echo "hello world"; %>'));
    }

    /**
     * test standard directory
     */
    public function testStandardDirectory()
    {
        $content = $this->smarty->fetch('eval:{include file="helloworld.tpl"}');
        $this->assertEquals("hello world", $content);
    }

    /**
     * test trusted directory
     */
    public function testTrustedDirectory()
    {
        $this->smarty->security_policy->secure_dir = array('.' . DIRECTORY_SEPARATOR . 'templates_2' . DIRECTORY_SEPARATOR);
        $this->assertEquals("hello world", $this->smarty->fetch('eval:{include file="templates_2/hello.tpl"}'));
    }

    /**
     * test not trusted directory
     */
    public function testNotTrustedDirectory()
    {
        $this->smarty->security_policy->secure_dir = array(str_replace('\\', '/', __DIR__ . '/templates_3/'));
        try {
            $this->smarty->fetch('eval:{include file="templates_2/hello.tpl"}');
        }
        catch (Exception $e) {
            $this->assertContains(str_replace('\\', '/', __DIR__ . "/templates_2/hello.tpl' not allowed by security setting"), str_replace('\\', '/', $e->getMessage()));

            return;
        }
        $this->fail('Exception for not trusted directory has not been raised.');
    }

    /**
     * test disabled security for not trusted dir
     */
    public function testDisabledTrustedDirectory()
    {
        $this->smarty->disableSecurity();
        $this->assertEquals("hello world", $this->smarty->fetch('eval:{include file="templates_2/hello.tpl"}'));
    }

    /**
     * test trusted static class
     */
    public function testTrustedStaticClass()
    {
        $this->smarty->security_policy->static_classes = array('mysecuritystaticclass');
        $tpl = $this->smarty->createTemplate('eval:{mysecuritystaticclass::square(5)}');
        $this->assertEquals('25', $this->smarty->fetch($tpl));
    }

    /**
     * test not trusted PHP function
     */
    public function testNotTrustedStaticClass()
    {
        $this->smarty->security_policy->static_classes = array('null');
        try {
            $this->smarty->fetch('eval:{mysecuritystaticclass::square(5)}');
        }
        catch (Exception $e) {
            $this->assertContains(htmlentities("access to static class 'mysecuritystaticclass' not allowed by security setting"), $e->getMessage());

            return;
        }
        $this->fail('Exception for not trusted static class has not been raised.');
    }

    public function testChangedTrustedDirectory()
    {
        $this->smarty->security_policy->secure_dir = array(
            '.' . DS . 'templates_2' . DS,
        );
        $this->assertEquals("hello world", $this->smarty->fetch('eval:{include file="templates_2/hello.tpl"}'));

        $this->smarty->security_policy->secure_dir = array(
            '.' . DS . 'templates_2' . DS,
            '.' . DS . 'templates_3' . DS,
        );
        $this->assertEquals("templates_3", $this->smarty->fetch('eval:{include file="templates_3/dirname.tpl"}'));
    }

    public function testTrustedUri()
    {
        $this->smarty->security_policy->trusted_uri = array(
            '#^http://.+smarty\.net$#i'
        );

        try {
            $this->smarty->fetch('eval:{fetch file="http://www.smarty.net/foo.bar"}');
        }
        catch (SmartyException $e) {
            $this->assertNotContains(htmlentities("not allowed by security setting"), $e->getMessage());
        }

        try {
            $this->smarty->fetch('eval:{fetch file="https://www.smarty.net/foo.bar"}');
            $this->fail("Exception for unknown resource not thrown (protocol)");
        }
        catch (SmartyException $e) {
            $this->assertContains(htmlentities("not allowed by security setting"), $e->getMessage());
        }

        try {
            $this->smarty->fetch('eval:{fetch file="http://www.smarty.com/foo.bar"}');
            $this->fail("Exception for unknown resource not thrown (domain)");
        }
        catch (SmartyException $e) {
            $this->assertContains(htmlentities("not allowed by security setting"), $e->getMessage());
        }
    }
}

class mysecuritystaticclass
{
    const STATIC_CONSTANT_VALUE = 3;
    static $static_var = 5;

    static function square($i)
    {
        return $i * $i;
    }
}
