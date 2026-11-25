<?php
/**
 * Smarty PHPunit tests of the {html_image} function plugin
 *
 * @package PHPunit
 */

/**
 * class for {html_image} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginFunctionHtmlImageTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Passing both width and height skips the getimagesize() lookup, so no real
     * image file is needed to render the tag.
     */
    private function render($params)
    {
        $tpl = $this->smarty->createTemplate('eval:{html_image file=$file width=$width height=$height href=$href path_prefix=$path_prefix}');
        $tpl->assign($params + array(
            'file'        => 'pic.jpg',
            'width'       => 44,
            'height'      => 68,
            'href'        => '',
            'path_prefix' => '',
        ));
        return $tpl->fetch();
    }

    public function testHrefIsEscaped()
    {
        $result = $this->render(array('href' => '"><script>alert(1)</script>'));
        $this->assertStringNotContainsString('<script>', $result);
        $this->assertStringContainsString('&lt;script&gt;', $result);
    }

    public function testWidthIsEscaped()
    {
        $result = $this->render(array('width' => '44" onload="alert(1)'));
        $this->assertStringNotContainsString('onload="', $result);
        $this->assertStringContainsString('&quot;', $result);
    }

    public function testHeightIsEscaped()
    {
        $result = $this->render(array('height' => '68" onmouseover="alert(1)'));
        $this->assertStringNotContainsString('onmouseover="', $result);
    }

    public function testFileAndPathPrefixAreEscaped()
    {
        $result = $this->render(array('file' => 'pic.jpg"><script>alert(1)</script>', 'path_prefix' => '"><b>'));
        $this->assertStringNotContainsString('<script>', $result);
        $this->assertStringNotContainsString('<b>', $result);
    }

    /**
     * Benign values must be unchanged (no breakage, no double-encoding of an
     * ampersand already present in a URL).
     */
    public function testBenignValuesAreUnchanged()
    {
        $result = $this->render(array('width' => 44, 'height' => 68, 'href' => 'detail.php?id=1&page=2'));
        $this->assertStringContainsString('width="44"', $result);
        $this->assertStringContainsString('height="68"', $result);
        $this->assertStringContainsString('src="pic.jpg"', $result);
        $this->assertStringContainsString('href="detail.php?id=1&amp;page=2"', $result);
        $this->assertStringNotContainsString('&amp;amp;', $result);
    }

    /**
     * Under a security policy a remote image must be read without following redirects, otherwise a redirect on a
     * trusted host reaches a URI that is not in trusted_uri (SSRF, CWE-918).
     */
    public function testRemoteImageDisablesRedirectsUnderSecurity()
    {
        HtmlImageContextCaptureStreamWrapper::$capturedOptions = null;
        stream_wrapper_register('ssrftest', HtmlImageContextCaptureStreamWrapper::class);
        try {
            $this->smarty->enableSecurity();
            $this->smarty->security_policy->trusted_uri[] = '/^ssrftest:\/\/allowed$/';

            $result = $this->smarty->fetch('string:{html_image file="ssrftest://allowed/pic.png"}');

            $this->assertStringContainsString('width="1" height="1"', $result);
            $this->assertSame(0, HtmlImageContextCaptureStreamWrapper::$capturedOptions['http']['follow_location']);
            $this->assertLessThanOrEqual(1, HtmlImageContextCaptureStreamWrapper::$capturedOptions['http']['max_redirects']);
        } finally {
            stream_wrapper_unregister('ssrftest');
        }
    }

    public function testRemoteImageWithoutSecurity()
    {
        HtmlImageContextCaptureStreamWrapper::$capturedOptions = null;
        stream_wrapper_register('ssrftest', HtmlImageContextCaptureStreamWrapper::class);
        try {
            $result = $this->smarty->fetch('string:{html_image file="ssrftest://allowed/pic.png"}');

            $this->assertStringContainsString('width="1" height="1"', $result);
        } finally {
            stream_wrapper_unregister('ssrftest');
        }
    }
}

/**
 * Records the stream context options {html_image} opens a resource with and serves a 1x1 PNG.
 */
class HtmlImageContextCaptureStreamWrapper
{
    /** @var resource|null set by PHP when a context is passed */
    public $context;

    /** @var array|null */
    public static $capturedOptions = null;

    private $data;

    public function stream_open($path, $mode, $options, &$opened_path)
    {
        self::$capturedOptions = isset($this->context) ? stream_context_get_options($this->context) : [];
        $this->data = base64_decode('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=');
        return true;
    }

    public function stream_read($count)
    {
        $chunk = substr($this->data, 0, $count);
        $this->data = (string) substr($this->data, $count);
        return $chunk;
    }

    public function stream_eof()
    {
        return $this->data === '';
    }

    public function stream_seek($offset, $whence)
    {
        return false;
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
