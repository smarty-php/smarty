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
        $this->setUpSmarty(dirname(__FILE__));

        $this->smarty->setForceCompile(true);
        $this->smarty->enableSecurity();
        $this->smartyBC->setForceCompile(true);
        $this->smartyBC->enableSecurity();
   }
   public function testInit()
    {
        $this->cleanDirs();
    }

/**
 * test that security is loaded
' *'/
    public function testSecurityReenable()
    {
        $this->smarty->disableSecurity();
        $this->smarty->enableSecurity('Security');
        $this->smarty->fetch('helloworld.tpl');
        $this->smarty->disableSecurity();
        $this->smarty->enableSecurity('Security');
        $this->smarty->fetch('helloworld.tpl');
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
        $this->assertEquals("5", $this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{count($foo)}'));
    }

/**
 * test not trusted PHP function
  * @expectedException        SmartyException
  * @expectedExceptionMessage   PHP function 'count' not allowed by security setting
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testNotTrustedPHPFunction()
    {
        $this->smarty->security_policy->php_functions = array('null');
            $this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{count($foo)}');
    }

/**
 * test not trusted PHP function at disabled security
 */
    public function testDisabledTrustedPHPFunction()
    {
        $this->smarty->security_policy->php_functions = array('null');
        $this->smarty->disableSecurity();
        $this->assertEquals("5", $this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{count($foo)}'));
    }

/**
 * test trusted modifier
 */
    public function testTrustedModifier()
    {
        $this->assertEquals("5", $this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{$foo|@count}'));
    }

/**
 * test not trusted modifier
  * @expectedException        SmartyException
  * @expectedExceptionMessage  modifier 'count' not allowed by security setting
  * @runInSeparateProcess
  * @preserveGlobalState disabled
 */
    public function testNotTrustedModifier()
    {
        $this->smarty->security_policy->php_modifiers = array('null');
        $this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{$foo|@count}');
    }

/**
 * test not trusted modifier at disabled security
 */
    public function testDisabledTrustedModifier()
    {
        $this->smarty->security_policy->php_modifiers = array('null');
        $this->smarty->disableSecurity();
        $this->assertEquals("5", $this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{$foo|@count}'));
    }

/**
 * test allowed tags
 */
    public function testAllowedTags1()
    {
        $this->smarty->security_policy->allowed_tags = array('counter');
        $this->assertEquals("1", $this->smarty->fetch('string:{counter start=1}'));
    }

/**
 * test not allowed tag
  * @expectedException        SmartyException
  * @expectedExceptionMessage   tag 'cycle' not allowed by security setting
  * @runInSeparateProcess
  * @preserveGlobalState disabled
 */
    public function testNotAllowedTags2()
    {
        $this->smarty->security_policy->allowed_tags = array('counter');
        $this->smarty->fetch('string:{counter}{cycle values="1,2"}');
    }

/**
 * test disabled tag
  * @expectedException        SmartyException
  * @expectedExceptionMessage  tag 'cycle' disabled by security setting
  * @runInSeparateProcess
  * @preserveGlobalState disabled
 */
    public function testDisabledTags()
    {
        $this->smarty->security_policy->disabled_tags = array('cycle');
        $this->smarty->fetch('string:{counter}{cycle values="1,2"}');
    }

/**
 * test allowed modifier
 */
    public function testAllowedModifier1()
    {
        error_reporting(E_ALL  & E_STRICT);
        $this->smarty->security_policy->allowed_modifiers = array('capitalize');
        $this->assertEquals("Hello World", $this->smarty->fetch('string:{"hello world"|capitalize}'));
        error_reporting(E_ALL | E_STRICT);
    }

    public function testAllowedModifier2()
    {
        $this->smarty->security_policy->allowed_modifiers = array('upper');
        $this->assertEquals("HELLO WORLD", $this->smarty->fetch('string:{"hello world"|upper}'));
    }

/**
 * test not allowed modifier
  * @expectedException        SmartyException
  * @expectedExceptionMessage   modifier 'lower' not allowed by security setting
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testNotAllowedModifier()
    {
        $this->smarty->security_policy->allowed_modifiers = array('upper');
        $this->smarty->fetch('string:{"hello"|upper}{"world"|lower}');
  }

/**
 * test disabled modifier
  * @expectedException        SmartyException
  * @expectedExceptionMessage   modifier 'lower' disabled by security setting
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testDisabledModifier()
    {
        $this->smarty->security_policy->disabled_modifiers = array('lower');
           $this->smarty->fetch('string:{"hello"|upper}{"world"|lower}');
    }

/**
 * test Smarty::PHP_QUOTE
 */
    public function testSmartyPhpQuote()
    {
        $this->smarty->security_policy->php_handling = Smarty::PHP_QUOTE;
        $this->assertEquals('&lt;?php echo "hello world"; ?&gt;', $this->smarty->fetch('string:<?php echo "hello world"; ?>'));
    }

    public function testSmartyPhpQuoteAsp()
    {
        // NOTE: asp_tags cannot be changed by ini_set()
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smarty->security_policy->php_handling = Smarty::PHP_QUOTE;
        $this->assertEquals('&lt;% echo "hello world"; %&gt;', $this->smarty->fetch('string:<% echo "hello world"; %>'));
    }

/**
 * test Smarty::PHP_REMOVE
 */
    public function testSmartyPhpRemove()
    {
        $this->smarty->security_policy->php_handling = Smarty::PHP_REMOVE;
        $this->assertEquals('', $this->smarty->fetch('string:<?php echo "hello world"; ?>'));
    }

    public function testSmartyPhpRemoveAsp()
    {
        // NOTE: asp_tags cannot be changed by ini_set()
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smarty->security_policy->php_handling = Smarty::PHP_REMOVE;
        $this->assertEquals('', $this->smarty->fetch('string:<% echo "hello world"; %>'));
    }

/**
 * test Smarty::PHP_ALLOW
 */
    public function testSmartyPhpAllow()
    {
        $this->smartyBC->security_policy->php_handling = Smarty::PHP_ALLOW;
        $this->assertEquals('hello world', $this->smartyBC->fetch('string:<?php echo "hello world"; ?>'));
    }

    public function testSmartyPhpAllowAsp()
    {
        // NOTE: asp_tags cannot be changed by ini_set()
        if (!ini_get('asp_tags')) {
            $this->markTestSkipped('asp tags disabled in php.ini');
        }
        $this->smartyBC->security_policy->php_handling = Smarty::PHP_ALLOW;
        $this->assertEquals('hello world', $this->smartyBC->fetch('string:<% echo "hello world"; %>'));
    }

/**
 * test standard directory
 */
    public function testStandardDirectory()
    {
        $content = $this->smarty->fetch('string:{include file="helloworld.tpl"}');
        $this->assertEquals("hello world", $content);
    }

/**
 * test trusted directory
 */
    public function testTrustedDirectory()
    {
        $this->smarty->security_policy->secure_dir = array('.' . DIRECTORY_SEPARATOR . 'templates_2' . DIRECTORY_SEPARATOR);
        $this->assertEquals("hello world", $this->smarty->fetch('string:{include file="templates_2/hello.tpl"}'));
    }

/**
 * test not trusted directory
  *
  * @expectedException        SmartyException
  * @expectedExceptionMessage   not trusted file path
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testNotTrustedDirectory()
    {
        $this->smarty->security_policy->secure_dir = array(str_replace('\\', '/', dirname(__FILE__) . '/templates_3/'));
             $this->smarty->fetch('string:{include file="templates_2/hello.tpl"}');
     }

/**
 * test disabled security for not trusted dir
 */
    public function testDisabledTrustedDirectory()
    {
        $this->smarty->disableSecurity();
        $this->assertEquals("hello world", $this->smarty->fetch('string:{include file="templates_2/hello.tpl"}'));
    }

/**
 * test trusted static class
 */
    public function testTrustedStaticClass()
    {
        $this->smarty->security_policy->static_classes = array('mysecuritystaticclass');
        $tpl = $this->smarty->createTemplate('string:{mysecuritystaticclass::square(5)}');
        $this->assertEquals('25', $this->smarty->fetch($tpl));
    }

/**
 * test not trusted PHP function
  * @expectedException        SmartyException
  * @expectedExceptionMessage   access to static class 'mysecuritystaticclass' not allowed by security setting
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testNotTrustedStaticClass()
    {
        $this->smarty->security_policy->static_classes = array('null');
            $this->smarty->fetch('string:{mysecuritystaticclass::square(5)}');
    }

    public function testChangedTrustedDirectory()
    {
        $this->smarty->security_policy->secure_dir = array(
            '.' . DIRECTORY_SEPARATOR . 'templates_2' . DIRECTORY_SEPARATOR,
        );
        $this->assertEquals("hello world", $this->smarty->fetch('string:{include file="templates_2/hello.tpl"}'));

        $this->smarty->security_policy->secure_dir = array(
            '.' . DIRECTORY_SEPARATOR . 'templates_2' . DIRECTORY_SEPARATOR,
            '.' . DIRECTORY_SEPARATOR . 'templates_3' . DIRECTORY_SEPARATOR,
        );
        $this->assertEquals("templates_3", $this->smarty->fetch('string:{include file="templates_3/dirname.tpl"}'));
    }
/**
 * test template file exits
 *
 * @runInSeparateProcess
 * @preserveGlobalState disabled
 */
    public function testTemplateTrustedStream()
    {
         stream_wrapper_register("global", "ResourceStreamSecurity")
        or die("Failed to register protocol");
        $fp = fopen("global://mytest", "r+");
        fwrite($fp, 'hello world {$foo}');
        fclose($fp);
        $this->smarty->security_policy->streams= array('global');
        $tpl = $this->smarty->createTemplate('global:mytest');
        $this->assertTrue($tpl->source->exists);
    }
/**
 * @expectedException        SmartyException
 * @expectedExceptionMessage  stream 'global' not allowed by security setting
 * @runInSeparateProcess
 * @preserveGlobalState disabled
 * test template file exits
 */
    public function testTemplateNotTrustedStream()
    {
        stream_wrapper_register("global", "ResourceStreamSecurity")
        or die("Failed to register protocol");
        $fp = fopen("global://mytest", "r+");
        fwrite($fp, 'hello world {$foo}');
        fclose($fp);
        $this->smarty->security_policy->streams= array('notrusted');
        $tpl = $this->smarty->createTemplate('global:mytest');
        $this->assertTrue($tpl->source->exists);
    }
/**
 * @runInSeparateProcess
 * @preserveGlobalState disabled
*/
    public function testTrustedUri()
    {
        $this->smarty->security_policy->trusted_uri = array(
            '#https://www.smarty.net$#i'
        );
        $this->assertContains('<title>Preface | Smarty</title>', $this->smarty->fetch('string:{fetch file="https://www.smarty.net/docs/en/preface.tpl"}'));
    }

/**
 * @expectedException        SmartyException
 * @expectedExceptionMessage  URI 'https://www.smarty.net/docs/en/preface.tpl' not allowed by security setting
 * @runInSeparateProcess
 * @preserveGlobalState disabled
*/
    public function testNotTrustedUri()
    {
        $this->smarty->security_policy->trusted_uri = array();
        $this->assertContains('<title>Preface | Smarty</title>', $this->smarty->fetch('string:{fetch file="https://www.smarty.net/docs/en/preface.tpl"}'));
    }

    /**
     * In security mode, accessing $smarty.template_object should be illegal.
     * @expectedException SmartyCompilerException
     */
    public function testSmartyTemplateObject() {
        $this->smarty->display('string:{$smarty.template_object}');
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
class Security extends Smarty_Security
{

}
class ResourceStreamSecurity
{
    private $position;
    private $varname;

    public function stream_open($path, $mode, $options, &$opened_path)
    {
        $url = parse_url($path);
        $this->varname = $url["host"];
        $this->position = 0;

        return true;
    }

    public function stream_read($count)
    {
        $p = &$this->position;
        $ret = substr($GLOBALS[$this->varname], $p, $count);
        $p += strlen($ret);

        return $ret;
    }

    public function stream_write($data)
    {
        $v = &$GLOBALS[$this->varname];
        $l = strlen($data);
        $p = &$this->position;
        $v = substr($v, 0, $p) . $data . substr($v, $p += $l);

        return $l;
    }

    public function stream_tell()
    {
        return $this->position;
    }

    public function stream_eof()
    {
        if (!isset($GLOBALS[$this->varname])) {
            return true;
        }

        return $this->position >= strlen($GLOBALS[$this->varname]);
    }

    public function stream_seek($offset, $whence)
    {
        $l = strlen($GLOBALS[$this->varname]);
        $p = &$this->position;
        switch ($whence) {
            case SEEK_SET:
                $newPos = $offset;
                break;
            case SEEK_CUR:
                $newPos = $p + $offset;
                break;
            case SEEK_END:
                $newPos = $l + $offset;
                break;
            default:
                return false;
        }
        $ret = ($newPos >= 0 && $newPos <= $l);
        if ($ret) {
            $p = $newPos;
        }
        return $ret;
    }
}
