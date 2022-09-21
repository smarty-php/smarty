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
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));

        $this->smarty->setForceCompile(true);
        $this->smarty->enableSecurity();
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
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testNotTrustedPHPFunction()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('PHP function \'count\' not allowed by security setting');
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
  * @runInSeparateProcess
  * @preserveGlobalState disabled
 */
    public function testNotTrustedModifier()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('modifier \'count\' not allowed by security setting');
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
  * @runInSeparateProcess
  * @preserveGlobalState disabled
 */
    public function testNotAllowedTags2()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('tag \'cycle\' not allowed by security setting');
        $this->smarty->security_policy->allowed_tags = array('counter');
        $this->smarty->fetch('string:{counter}{cycle values="1,2"}');
    }

/**
 * test disabled tag
  * @runInSeparateProcess
  * @preserveGlobalState disabled
 */
    public function testDisabledTags()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('tag \'cycle\' disabled by security setting');
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
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testNotAllowedModifier()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('modifier \'lower\' not allowed by security setting');
        $this->smarty->security_policy->allowed_modifiers = array('upper');
        $this->smarty->fetch('string:{"hello"|upper}{"world"|lower}');
    }

/**
 * test disabled modifier
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testDisabledModifier()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('modifier \'lower\' disabled by security setting');
        $this->smarty->security_policy->disabled_modifiers = array('lower');
        $this->smarty->fetch('string:{"hello"|upper}{"world"|lower}');
    }


/**
 * test Smarty no longer handles embedded PHP
 */
    public function testSmartyPhpAllow()
    {
        $this->assertEquals('<?php echo "hello world"; ?>', $this->smarty->fetch('string:<?php echo "hello world"; ?>'));
    }

    public function testSmartyPhpAllow2()
    {
        $this->assertEquals('<? echo "hello world"; ?>', $this->smarty->fetch('string:<? echo "hello world"; ?>'));
    }

    public function testSmartyPhpAllow3()
    {
        $this->assertEquals('<% echo "hello world"; %>', $this->smarty->fetch('string:<% echo "hello world"; %>'));
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
  * @runInSeparateProcess
  * @preserveGlobalState disabled
  */
    public function testNotTrustedDirectory()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('not trusted file path');
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
	 * @runInSeparateProcess
	 * @preserveGlobalState disabled
	 */
	public function testNotTrustedStaticClass()
	{
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('access to static class \'mysecuritystaticclass\' not allowed by security setting');
        $this->smarty->security_policy->static_classes = array('null');
        $this->smarty->fetch('string:{mysecuritystaticclass::square(5)}');
    }

	/**
	 * test not trusted PHP function
	 */
	public function testNotTrustedStaticClassEval()
	{
		$this->expectException('SmartyException');
		$this->expectExceptionMessage('dynamic static class not allowed by security setting');
		$this->smarty->security_policy->static_classes = array('null');
		$this->smarty->fetch('string:{$test = "mysecuritystaticclass"}{$test::square(5)}');
	}

	/**
	 * test not trusted PHP function
	 */
	public function testNotTrustedStaticClassSmartyVar()
	{
		$this->expectException('SmartyException');
		$this->expectExceptionMessage('dynamic static class not allowed by security setting');
		$this->smarty->security_policy->static_classes = array('null');
		$this->smarty->fetch('string:{$smarty.template_object::square(5)}');
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
 * @runInSeparateProcess
 * @preserveGlobalState disabled
 * test template file exits
 */
    public function testTemplateNotTrustedStream()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('stream \'global\' not allowed by security setting');
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
        $this->assertStringContainsString('<title>Preface | Smarty</title>', $this->smarty->fetch('string:{fetch file="https://www.smarty.net/docs/en/preface.tpl"}'));
    }

/**
 * @runInSeparateProcess
 * @preserveGlobalState disabled
*/
    public function testNotTrustedUri()
    {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('URI \'https://www.smarty.net/docs/en/preface.tpl\' not allowed by security setting');
        $this->smarty->security_policy->trusted_uri = array();
        $this->assertStringContainsString('<title>Preface | Smarty</title>', $this->smarty->fetch('string:{fetch file="https://www.smarty.net/docs/en/preface.tpl"}'));
    }

    /**
     * In security mode, accessing $smarty.template_object should be illegal.
     */
    public function testSmartyTemplateObject() {
        $this->expectException(SmartyCompilerException::class);
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
        $v = substr($v ?? '', 0, $p) . $data . substr($v ?? '', $p += $l);

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
