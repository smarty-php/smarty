<?php
/**
 * Smarty PHPunit tests stream variables
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for stream variables tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class StreamVariableTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));

        stream_wrapper_register("var", "VariableStream")
        or die("Failed to register protocol");
        $fp = fopen("var://foo", "r+");
        fwrite($fp, 'hello world');
        fclose($fp);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function tearDown()
    {
        parent::tearDown();
        stream_wrapper_unregister("var");
    }

    /**
     * test stream variable
     */
    public function testStreamVariable1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$var:foo}', null, null, $this->smarty);
        $this->assertEquals('hello world', $this->smarty->fetch($tpl));
    }
    /*
        public function testStreamVariable2()
        {
            $tpl = $this->smarty->createTemplate('eval:{var:\'foo\'}', null, null, $this->smarty);
            $this->assertEquals('hello world', $this->smarty->fetch($tpl));
        }

        public function testStreamVariable3()
        {
            $tpl = $this->smarty->createTemplate('eval:{var:"foo"}', null, null, $this->smarty);
            $this->assertEquals('hello world', $this->smarty->fetch($tpl));
        }
    */
    /**
     * test no existent stream variable
     */
    //    public function testStreamVariable2()
    //    {
    //        $tpl = $this->smarty->createTemplate('eval:{$var:bar}', null, null, $this->smarty);
    //        $this->assertEquals('', $this->smarty->fetch($tpl));
    //    }
}

class VariableStream
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
