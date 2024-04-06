<?php
/**
 * Smarty PHPunit tests for security
 *

 * @author  Uwe Tews
 */

use Smarty\CompilerException;

/**
 * class for security test
 */
class SecurityTest extends PHPUnit_Smarty
{
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);

		$this->smarty->setForceCompile(true);
		$this->smarty->enableSecurity();
	}
	public function testInit()
	{
		$this->cleanDirs();
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
	public function testTrustedFunction()
	{
		$this->assertEquals("5", $this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{count($foo)}'));
	}

	/**
	 * test trusted modifier
	 * @deprecated
	 */
	public function testTrustedModifier()
	{
		$this->assertEquals("5", @$this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{$foo|@count}'));
	}

	/**
	 * test not trusted modifier
	 *
	 *
	 *  @deprecated
	 */
	public function testNotTrustedModifier()
	{
		$this->smarty->security_policy->disabled_modifiers[] = 'escape';
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('modifier \'escape\' disabled by security setting');
		@$this->smarty->fetch('string:{assign var=foo value=[1,2,3,4,5]}{$foo|escape}');
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
	 *
	 *
	 */
	public function testNotAllowedTags2()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('tag \'cycle\' not allowed by security setting');
		$this->smarty->security_policy->allowed_tags = array('counter');
		$this->smarty->fetch('string:{counter}{cycle values="1,2"}');
	}

	/**
	 * test disabled tag
	 *
	 *
	 */
	public function testDisabledTags()
	{
		$this->expectException(\Smarty\Exception::class);
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
	 *
	 *
	 */
	public function testNotAllowedModifier()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('modifier \'lower\' not allowed by security setting');
		$this->smarty->security_policy->allowed_modifiers = array('upper');
		$this->smarty->fetch('string:{"hello"|upper}{"world"|lower}');
	}

	/**
	 * test disabled modifier
	 *
	 *
	 */
	public function testDisabledModifier()
	{
		$this->expectException(\Smarty\Exception::class);
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
	 *
	 *
	 */
	public function testNotTrustedDirectory()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('not trusted file path');
		$this->smarty->security_policy->secure_dir = array(str_replace('\\', '/', __DIR__ . '/templates_3/'));
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
	 *
	 *
	 */
	public function testNotTrustedStaticClass()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('access to static class \'mysecuritystaticclass\' not allowed by security setting');
		$this->smarty->security_policy->static_classes = array('null');
		$this->smarty->fetch('string:{mysecuritystaticclass::square(5)}');
	}

	/**
	 * test not trusted PHP function
	 */
	public function testNotTrustedStaticClassEval()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('dynamic static class not allowed by security setting');
		$this->smarty->security_policy->static_classes = array('null');
		$this->smarty->fetch('string:{$test = "mysecuritystaticclass"}{$test::square(5)}');
	}

	/**
	 * test not trusted PHP function
	 */
	public function testNotTrustedStaticClassSmartyVar()
	{
		$this->expectException(\Smarty\Exception::class);
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
	 *
	 *
	 */
	public function testTemplateTrustedStream()
	{
		stream_wrapper_register("global", ResourceStreamSecurity::class)
		or die("Failed to register protocol");
		$fp = fopen("global://mytest", "r+");
		fwrite($fp, 'hello world {$foo}');
		fclose($fp);
		$this->smarty->security_policy->streams= array('global');
		$tpl = $this->smarty->createTemplate('global:mytest');
		$this->assertTrue($tpl->getSource()->exists);
		stream_wrapper_unregister("global");
	}
	/**
	 *
	 *
	 * test template file exits
	 */
	public function testTemplateNotTrustedStream()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('stream \'global\' not allowed by security setting');
		stream_wrapper_register("global", ResourceStreamSecurity::class)
		or die("Failed to register protocol");
		$fp = fopen("global://mytest", "r+");
		fwrite($fp, 'hello world {$foo}');
		fclose($fp);
		$this->smarty->security_policy->streams= array('notrusted');
		$tpl = $this->smarty->createTemplate('global:mytest');
		$this->assertTrue($tpl->getSource()->exists);
		stream_wrapper_unregister("global");
	}

	public function testTrustedUri()
	{
		$this->smarty->security_policy->trusted_uri = array(
			'#https://s4otw4nhg.erteorteortert.nusuchtld$#i'
		);

		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('{fetch} cannot read resource \'https://s4otw4nhg.erteorteortert.nusuchtld/docs/en/preface.tpl\'');

		$this->smarty->fetch('string:{fetch file="https://s4otw4nhg.erteorteortert.nusuchtld/docs/en/preface.tpl"}');
	}

	/**
	 *
	 *
	 */
	public function testNotTrustedUri()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('URI \'https://example.net\' not allowed by security setting');
		$this->smarty->security_policy->trusted_uri = [];
		$this->assertStringContainsString(
			'<title>Preface | Smarty</title>',
			$this->smarty->fetch('string:{fetch file="https://example.net"}')
		);
	}

	/**
	 * In security mode, accessing $smarty.template_object should be illegal.
	 */
	public function testSmartyTemplateObject() {
		$this->expectException(CompilerException::class);
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

#[AllowDynamicProperties]
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
