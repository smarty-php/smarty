<?php

/**
 * class testing fetch function
 */
class PluginFunctionFetchTest extends PHPUnit_Smarty
{
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}


	/**
	 * test {fetch} from local file
	 */
	public function testFetchFile()
	{
		$this->assertStringContainsString(
			'ct4hn8nzgm;cgzm;',
			$this->smarty->fetch('string:{fetch file="./testfile.txt"}')
		);
	}

	/**
	 * test {fetch} non-existing file
	 */
	public function testFetchNonExistingFile()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('{fetch} cannot read resource \'./no/such/file\'');
		$this->smarty->fetch('string:{fetch file="./no/such/file"}');
	}

	/**
	 * test {fetch file=...} access to file from path not aloo/wed by security settings
	 *
	 * @run InSeparateProcess
	 *
	 */
	public function testFetchSecurity()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('not trusted file path');
		$this->cleanDirs();
		$dir=$this->smarty->getTemplateDir();
		$this->smarty->enableSecurity();
		$this->smarty->fetch('string:{fetch file=\''. $dir[0]. '../../../../../etc/passwd\'}');
	}
	/**
	 * test {fetch file=...} access to file from path not aloo/wed by security settings
	 *
	 * @run InSeparateProcess
	 *
	 */
	public function testFetchSecurity2()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('not trusted file path');
		$this->cleanDirs();
		$this->smarty->getTemplateDir();
		$this->smarty->enableSecurity();
		$this->smarty->setTemplateDir('/templates');
		$this->smarty->fetch('string:{fetch file="/templates/../etc/passwd"}');
	}

	/**
	 * When a security policy is in effect, {fetch} of a remote resource must not
	 * follow redirects, otherwise an open redirect on a trusted host could be
	 * used to bypass trusted_uri and reach an internal target (SSRF, CWE-918).
	 */
	public function testFetchRemoteDisablesRedirectsUnderSecurity()
	{
		FetchContextCaptureStreamWrapper::$capturedOptions = null;
		stream_wrapper_register('ssrftest', FetchContextCaptureStreamWrapper::class);
		try {
			$this->smarty->enableSecurity();
			$this->smarty->security_policy->trusted_uri[] = '/^ssrftest:\/\/allowed$/';

			$result = $this->smarty->fetch('string:{fetch file="ssrftest://allowed/data"}');

			$this->assertSame('BODY', $result);
			$this->assertIsArray(FetchContextCaptureStreamWrapper::$capturedOptions);
			$this->assertArrayHasKey('http', FetchContextCaptureStreamWrapper::$capturedOptions);
			$this->assertSame(0, FetchContextCaptureStreamWrapper::$capturedOptions['http']['follow_location']);
			$this->assertLessThanOrEqual(1, FetchContextCaptureStreamWrapper::$capturedOptions['http']['max_redirects']);
		} finally {
			stream_wrapper_unregister('ssrftest');
		}
	}

	/**
	 * Without a security policy there is no trusted_uri to bypass, so the
	 * redirect-disabling stream context is not applied (backwards compatible).
	 */
	public function testFetchRemoteKeepsDefaultBehaviorWithoutSecurity()
	{
		FetchContextCaptureStreamWrapper::$capturedOptions = null;
		stream_wrapper_register('ssrftest', FetchContextCaptureStreamWrapper::class);
		try {
			$result = $this->smarty->fetch('string:{fetch file="ssrftest://allowed/data"}');

			$this->assertSame('BODY', $result);
			$this->assertSame([], FetchContextCaptureStreamWrapper::$capturedOptions);
		} finally {
			stream_wrapper_unregister('ssrftest');
		}
	}

}

/**
 * Minimal custom stream wrapper used by the fetch SSRF tests: it records the
 * stream context options that {fetch} passes to file_get_contents() and returns
 * a fixed body so the call succeeds without touching the network.
 */
class FetchContextCaptureStreamWrapper
{
	/** @var resource|null populated by PHP when a context is passed */
	public $context;

	/** @var array|null options captured from the context on the last open */
	public static $capturedOptions = null;

	private $read = false;

	public function stream_open($path, $mode, $options, &$opened_path)
	{
		self::$capturedOptions = isset($this->context) ? stream_context_get_options($this->context) : [];
		return true;
	}

	public function stream_read($count)
	{
		if ($this->read) {
			return '';
		}
		$this->read = true;
		return 'BODY';
	}

	public function stream_eof()
	{
		return $this->read;
	}

	public function stream_stat()
	{
		return [];
	}

	public function url_stat($path, $flags)
	{
		return [];
	}
}
