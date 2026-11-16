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
}
