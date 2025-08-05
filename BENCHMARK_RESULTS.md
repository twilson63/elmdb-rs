# ElmDB-RS Optimized Performance Benchmark Results

## Test Configuration

- **Database**: ElmDB-RS (Rust NIF LMDB bindings)
- **Test Environment**: macOS Darwin 24.5.0
- **Record Count**: 100,000 records per test
- **Optimizations Applied**:
  - `no_sync`: Don't flush system buffers to disk when committing
  - `no_mem_init`: Don't initialize malloc'd memory before writing
  - `write_map`: Use writeable memory map for better performance
- **Map Size**: 2GB
- **Record Size**: ~50 bytes per key-value pair (including variable data)

## Performance Results

### Write Performance
- **Average Write Rate**: ~205,000 records/second
- **Time per Record**: ~4.9 microseconds
- **Total Time (100K records)**: ~0.49 seconds
- **Estimated Throughput**: ~9.8 MB/second
- **Success Rate**: 100% (no failed writes)

### Read Performance
- **Average Read Rate**: ~3,230,000 records/second
- **Time per Record**: ~0.31 microseconds
- **Total Time (100K records)**: ~0.031 seconds
- **Estimated Throughput**: ~154 MB/second
- **Success Rate**: 100% (no failed reads)

### Performance Ratios
- **Read/Write Ratio**: 15.8x (reads are ~16x faster than writes)

## Detailed Results (Multiple Runs)

| Iteration | Write Rate (rec/sec) | Read Rate (rec/sec) | Read/Write Ratio |
|-----------|---------------------|---------------------|------------------|
| 1         | 204,649            | 3,169,773          | 15.49x          |
| 2         | 203,572            | 3,201,332          | 15.73x          |
| 3         | 207,043            | 3,294,025          | 15.91x          |
| **Average** | **205,088**        | **3,221,710**      | **15.71x**      |

## Performance Assessment

### Write Performance: EXCELLENT
- Achieved >200K records/second (well above 100K threshold for excellent)
- Consistent performance across multiple runs
- Zero write failures

### Read Performance: EXCELLENT  
- Achieved >3.2M records/second (well above 500K threshold for excellent)
- Exceptional read speed due to memory-mapped access
- Zero read failures

## Optimization Impact

The optimized settings provide significant performance benefits:

1. **no_sync**: Eliminates disk sync overhead during writes, dramatically improving write throughput
2. **no_mem_init**: Reduces memory initialization overhead
3. **write_map**: Enables direct memory-mapped writes, improving both read and write performance

**Trade-off**: These optimizations prioritize performance over durability. In case of system crash, recent writes might be lost.

## Comparison with Industry Standards

- **SQLite**: Typically 10K-50K writes/sec, 100K-500K reads/sec
- **RocksDB**: Typically 100K-300K writes/sec, 500K-1M reads/sec
- **ElmDB-RS (optimized)**: 205K writes/sec, 3.2M reads/sec

ElmDB-RS shows excellent performance, especially for read operations, demonstrating the benefits of LMDB's memory-mapped architecture combined with Rust's efficiency.

## Recommendations

### For Production Use:
1. **High-Read Workloads**: Excellent choice due to exceptional read performance
2. **Balanced Workloads**: Good performance for both reads and writes
3. **Durability Requirements**: Consider disabling `no_sync` if crash recovery is critical
4. **Memory Usage**: Monitor memory usage with large datasets due to memory mapping

### For Development:
1. Use the optimized settings for development and testing
2. Benchmark with your specific data patterns and sizes
3. Test with concurrent access patterns if applicable

## Technical Notes

- LMDB's copy-on-write B+ trees provide excellent read performance
- Memory-mapped files eliminate buffer copies
- The Rust NIF implementation adds minimal overhead
- BEAM VM's lightweight processes enable efficient concurrent access

---

**Generated**: 2025-08-03  
**Benchmark Script**: `/Users/rakis/code/m3/elmdb-rs/benchmark_optimized.erl`