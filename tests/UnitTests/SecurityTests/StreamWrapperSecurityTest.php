<?php
/**
 * Smarty PHPunit tests for stream-wrapper security
 *
 * @package PHPunit
 */

/**
 * Regression tests ensuring the built-in "stream" resource type cannot be used
 * to bypass the stream-wrapper restrictions enforced by Smarty Security.
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class StreamWrapperSecurityTest extends PHPUnit_Smarty
{
    private $secretFile;

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->secretFile = sys_get_temp_dir() . DIRECTORY_SEPARATOR
            . 'smarty_stream_secret_' . getmypid() . '_' . uniqid() . '.txt';
        file_put_contents($this->secretFile, 'STREAM-WRAPPER-SECRET');
        $this->smarty->setForceCompile(true);
        $this->smarty->enableSecurity();
    }

    public function tearDown(): void
    {
        if ($this->secretFile && file_exists($this->secretFile)) {
            unlink($this->secretFile);
        }
        parent::tearDown();
    }

    private function phpFilterUri()
    {
        return 'php://filter/read=convert.base64-encode/resource=' . $this->secretFile;
    }

    /**
     * Sanity: a direct php:// stream is rejected when all streams are disabled.
     */
    public function testDirectPhpStreamIsBlocked()
    {
        $this->smarty->security_policy->streams = null;
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage("stream 'php' not allowed by security setting");
        $this->smarty->fetch('string:{include file="' . $this->phpFilterUri() . '"}');
    }

    /**
     * The built-in "stream" resource type must not let a nested php:// wrapper
     * escape the same restriction (CWE-22 / wrapper bypass).
     */
    public function testStreamResourceCannotBypassDisabledStreams()
    {
        $this->smarty->security_policy->streams = null;
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage("stream 'php' not allowed by security setting");
        $this->smarty->fetch('string:{include file="stream:' . $this->phpFilterUri() . '"}');
    }

    /**
     * Even when some streams are allowed, a nested wrapper that is not on the
     * allowlist must still be rejected through the "stream" resource type.
     */
    public function testStreamResourceRejectsWrapperNotOnAllowlist()
    {
        $this->smarty->security_policy->streams = array('file');
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage("stream 'php' not allowed by security setting");
        $this->smarty->fetch('string:{include file="stream:' . $this->phpFilterUri() . '"}');
    }

    /**
     * A wrapper explicitly allowed by the policy must keep working through the
     * "stream" resource type (no backwards-compatibility break).
     */
    public function testStreamResourceAllowsWhitelistedWrapper()
    {
        stream_wrapper_register('smartyteststream', 'StreamSecurityTestWrapper');
        try {
            $this->smarty->security_policy->streams = array('smartyteststream');
            $this->smarty->assign('name', 'World');
            $result = $this->smarty->fetch('string:{include file="stream:smartyteststream://x"}');
            $this->assertEquals('hello World', $result);
        } finally {
            stream_wrapper_unregister('smartyteststream');
        }
    }
}

/**
 * Minimal read-only stream wrapper returning a fixed template body, used by the
 * allowlist (positive) test above.
 */
#[AllowDynamicProperties]
class StreamSecurityTestWrapper
{
    public $context;
    private $pos = 0;
    private $data = 'hello {$name}';

    public function stream_open($path, $mode, $options, &$opened_path)
    {
        $this->pos = 0;
        return true;
    }

    public function stream_read($count)
    {
        $ret = substr($this->data, $this->pos, $count);
        $this->pos += strlen($ret);
        return $ret;
    }

    public function stream_eof()
    {
        return $this->pos >= strlen($this->data);
    }

    public function stream_stat()
    {
        return array();
    }

    public function url_stat($path, $flags)
    {
        return array();
    }

    public function stream_seek($offset, $whence)
    {
        return false;
    }
}
