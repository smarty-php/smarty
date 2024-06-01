<?php declare(strict_types=1);

namespace Smarty\Extension\DefaultExtension;

use Smarty\Exception;

class RecursiveTranscoder {

	/**
	 * Determines if the given value is a possible candidate for transcoding.
	 *
	 * @param mixed $value
	 * @return bool
	 */
	public static function is_transcoding_candidate($value): bool {
		if (empty($value)) {
			return false;
		}
		return is_string($value) || is_array($value) || is_object($value);
	}


	/**
	 * Similar to mb_convert_encoding(), but operates recursively on keys and values of arrays, and on objects too.
	 * Objects implementing \JsonSerializable and unsupported types are returned unchanged.
	 * The following boolean options/hints are supported (all default to false):
	 *	- ignore_keys: do not transcode array keys
	 *	- ignore_objects: leave objects alone, i.e. do not convert them into associative arrays and transcode them
	 *	- ignore_JsonSerializable_objects: if transcoded result is meant as input for json_encode(), then set this to true.
	 *
	 * @param mixed $data
	 * @param string $to_encoding
	 * @param string $from_encoding of $data; defaults to \Smarty\Smarty::$_CHARSET
	 * @param array $options
	 * @return mixed
	 */
	public static function transcode($data, string $to_encoding, string $from_encoding = null, array $options = null) {
		$debug = !empty($options['debug']);
		#$debug = true;
		$debug && error_log(__METHOD__ . ' entered with $data type ' . gettype($data));
		if (!static::is_transcoding_candidate($data)) {
			$debug && error_log(__METHOD__ . ' return $data unchanged since it is not a transcoding candidate');
			return $data;
		}
		if (!$from_encoding) {
			$from_encoding = \Smarty\Smarty::$_CHARSET;
		}
		if (strcasecmp($to_encoding, $from_encoding) == 0) {
			$debug && error_log(__METHOD__ . ' return $data unchanged since to_encoding = from_encoding');
			return $data;
		}

		# most cases:
		if (is_string($data)) {
			$debug && error_log(__METHOD__ . ' return mb_convert_encoding string $data');
			return mb_convert_encoding($data, $to_encoding, $from_encoding);	# string|false
		}

		# convert object to array to be transcoded as array
		if (is_object($data)) {
			if (!empty($options['ignore_objects'])) {
				$debug && error_log(__METHOD__ . ' return object unchanged since ignore_objects is true');
				return $data;
			}
			if (is_a($data, \JsonSerializable::class)) {
				if (!empty($options['ignore_JsonSerializable_objects'])) {
					$debug && error_log(__METHOD__ . ' return JsonSerializable object unchanged since ignore_JsonSerializable_objects is true');
					return $data;	# \JsonSerializable objects should be trusted to serialize themselves into data that can be consumed by json_encode() no matter what the application's default encoding is.
				}
				#$debug && error_log(__METHOD__ . ' call jsonSerialize() on JsonSerializable object');
				#$data = $data->jsonSerialize();	# mixed
				#if (!is_array($data)) {
				#	$debug && error_log(__METHOD__ . ' return non-array jsonSerialize() on JsonSerializable object');
				#	return $data;
				#}
			}
			$debug && error_log(__METHOD__ . ' convert object to array');
			$data = get_object_vars($data); # public properties as key => value pairs
		}

		if (!(is_array($data) && $data)) {
			$debug && error_log(__METHOD__ . ' return non-array or empty empty');
			return $data;	# any empty array or non-array type as a possible result of object conversion above
		}

		# $data is a filled array
		$must_transcode_keys = empty($options['ignore_keys']);
		$debug && error_log(__METHOD__ . " process filled array (must_transcode_keys == $must_transcode_keys)");
		$result = [];	# replacement for $data if keys are transcoded too (i.e. $must_transcode_keys)
		$this_func = __FUNCTION__;	# for recursion
		foreach ($data as $k => &$v) {
			if ($must_transcode_keys && is_string($k)) {
				$converted_k = mb_convert_encoding($k, $to_encoding, $from_encoding);	# string|false
				$debug && error_log(__METHOD__ . "\tconverted key \"$k\" to \"$converted_k\"");
				if ($converted_k === false) {	# this means mb_convert_encoding() failed which should've triggered a warning
					# One of three things can be done here:
					# 1. throw an Exception
					# 2. return false, indicating to caller that mb_convert_encoding() failed
					# 3. do nothing and use the original key
					#return false;
					throw Exception("Failed to encode array key \"$k\" from $from_encoding to $to_encoding");
				}
				else {
					$k = $converted_k;
				}
			}
			if (static::is_transcoding_candidate($v)) {
				$debug && error_log(__METHOD__ . "\trecurse for " . gettype($value) . ' value that is a transcoding candidate');
				# recurse
				$converted_v = static::$this_func($v, $to_encoding, $from_encoding, $options);
				if ($converted_v === false) {	# this means that $v is a string and that mb_convert_encoding() failed, which should've triggered a warning
					# One of four things can be done here:
					# 1. throw an Exception
					# 2. return false, indicating to caller that mb_convert_encoding() failed
					# 3. do nothing and use the original value
					# 4. replace the original value with false
					#return false;
					throw Exception('Failed to encode array value' . (is_string($v) ? " \"$k\"" : '')  . 'of type ' . gettype($v) . " from $from_encoding to $to_encoding");
				}
				else {
					$v = $converted_v;
					if ($must_transcode_keys) {
						$result[$k] = $v;
					}
				}
			}
			else {
				# $v may be false here, and in this case it is not an error (since no transcoding occurred since it's not a transcoding candidate)
				if ($must_transcode_keys) {
					$result[$k] = $v;
				}
			}
			unset($v);
		}
		return $must_transcode_keys ? $result : $data;
	}

}
