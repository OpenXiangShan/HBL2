# XSCache PR #14 迁移说明

本文档记录将 https://github.com/OpenXiangShan/XSCache/pull/14
迁移到当前 CoupledL2 分支时实际改了什么、没有改什么，以及对应原因。

## 改了哪些内容

### 1. 新增 ZhuJiang submodule

- 在 `.gitmodules` 中新增 `ZhuJiang` submodule。
- 将 `ZhuJiang` 固定到提交 `8a567f75cc4ec468e7ad865796af7bd2462d2c70`，也就是原 PR 最终使用的 ZhuJiang 提交。

原因：新增的测试顶层会直接实例化 ZhuJiang 模块，同时依赖 ZhuJiang 仓库里的 `xs-utils` 源码。

### 2. 给 TL2CHICoupledL2 增加 decoupled downstream CHI 模式

- 在 `src/main/scala/coupledL2/L2Param.scala` 中新增 `EnableL2DecoupledDownstreamCHI` 参数。
- 修改 `src/main/scala/coupledL2/tl2chi/TL2CHICoupledL2.scala`，让下游 CHI 端口支持两种模式：
  - 默认模式：继续使用原来的 L-credit `PortIO`，经过 `LinkMonitor`。
  - ZhuJiang 模式：使用直接的 `DecoupledPortIO`。
- 修改 `src/test/scala/chi/TestTop.scala`，因为 `io_chi` 变成了可选端口，所以现有 CHI test top 通过 `io_chi.get` 访问默认 L-credit 端口。

原因：ZhuJiang socket 接口是 decoupled 形式，而当前已有 CHI test top 仍依赖 L-credit CHI 端口。把 decoupled 端口做成 opt-in，可以新增 ZhuJiang 支持，同时不改变默认行为。

### 3. 新增 ZhuJiang 测试顶层和桥接逻辑

- 新增 `src/test/scala/TestTopZhuJiang.scala`。
- 新增 `src/test/scala/ZhuJiangBridge.scala`。
- 新测试顶层做了这些事：
  - 按 core 数实例化 `TL2CHICoupledL2`。
  - 为这些 L2 打开 `EnableCHI` 和 `EnableL2DecoupledDownstreamCHI`。
  - 实例化 ZhuJiang，并提供 single-core / dual-core 两套较小的 NoC 配置。
  - 将 CoupledL2 的 CHI 请求接到 ZhuJiang CC socket。
  - 暴露 TileLink L1 端口和 DDR AXI memory 端口。
- 桥接文件负责：
  - CoupledL2 CHI request/response/data/snoop flit 和 ZhuJiang flit 之间的字段映射。
  - ZhuJiang AXI 端口和 RocketChip AXI4 端口之间的连接。

原因：原 PR 的核心目标是提供 CoupledL2 + ZhuJiang 的可生成测试顶层。本仓库包名和入口与 XSCache PR 基线不同，所以这里按当前仓库的 `coupledL2` / `TL2CHICoupledL2` 结构做了适配。

### 4. 更新 Mill 和 Makefile 目标

- 修改 `build.sc`：
  - 保留普通 `CoupledL2.test`，不让它无条件依赖 ZhuJiang。
  - 新增 `CoupledL2.testtop.l2`，用于现有 L2 / CHI test top。
  - 新增 `CoupledL2.testtop.zhujiang`，只编译 ZhuJiang test top 相关源码。
  - 新增 `zhujiangCompat`，把 `ZhuJiang/src/main/scala` 和 `ZhuJiang/xs-utils/src/main/scala` 纳入当前仓库构建。
- 修改 `Makefile`：
  - `gen-test-top` 和 `gen-test-top-chi` 改为使用 `CoupledL2.testtop.l2`。
  - 新增 `test-top-zhujiang-singlecore`。
  - 新增 `test-top-zhujiang-dualcore`。

原因：如果把 ZhuJiang 测试直接放进默认 test 模块，会让所有测试编译都依赖 ZhuJiang，影响原有轻量测试路径。拆成单独 Mill module 后，依赖关系更明确。

### 5. 更新 CI

- 修改 `.github/workflows/main.yml`：
  - 增加 ZhuJiang submodule ancestry 检查。
  - 增加 ZhuJiang 测试 artifact 名称。
  - 增加 `CoupledL2 + ZhuJiang` 的 `tl-test-new` 测试步骤，使用 `feat-zhujiang` 分支。

原因：原 PR 增加了新集成路径的 CI 覆盖。这里保留当前仓库已有 CI 结构，只追加 ZhuJiang 相关检查和测试。

## 哪些内容没有改

### 1. 没有把仓库 / Mill root 改名为 XSCache

- 没有把 `CoupledL2` Mill root 改成 `XSCache`。
- 没有引入 `xscache.*` 包名。

原因：当前分支仍然是 HBL2/CoupledL2 结构。全局改名会扩大迁移范围，并破坏已有本地目标。

### 2. 没有迁移 OpenLLC 相关重命名和目标

- 没有照搬原 PR 中 `TestTopOpenLLC` 相关重命名。
- 没有新增 OpenLLC 专用 Make target。

原因：当前仓库没有和 XSCache PR 基线一致的 `TestTop_LLC.scala` / OpenLLC 文件布局。ZhuJiang 集成不依赖这些改动。

### 3. 没有照搬 CI branch filter 改动

- 没有移除当前 CI 里已有的 `chi-coupledl2` branch filter。

原因：branch filter 属于仓库策略，不是 ZhuJiang 功能迁移的必要部分。这里保持当前仓库策略，只追加 ZhuJiang 覆盖。

### 4. 没有替换原有 L-credit CHI 端口

- 没有删除 `LinkMonitor`。
- 没有把已有 CHI test top 改成 decoupled CHI。

原因：现有 CHI test top 和 CHI logging 仍使用 L-credit 端口。decoupled CHI 只在 `EnableL2DecoupledDownstreamCHI=true` 时启用。

### 5. 没有直接复用 ZhuJiang 自己的 build.sc

- 没有把 ZhuJiang 的独立 Mill 构建完整接入。

原因：ZhuJiang 自己的 `build.sc` 定义了一套独立 RocketChip / xs-utils module graph 和版本。当前迁移只需要 ZhuJiang 源码参与本仓库编译，因此使用 `zhujiangCompat` 更小、更可控。

### 6. 没有跑 dual-core 生成验证

- 没有执行 `make test-top-zhujiang-dualcore`。

原因：这次验证优先选择 `singlecore`，因为它已经覆盖新增 submodule、decoupled CHI 路径、ZhuJiang bridge 和 AXI 连接，运行成本更低。dual-core 目标已经添加，但未在本次验证中生成。

## 已验证内容

迁移后运行并通过了以下命令：

- `mill -i CoupledL2.compile`
- `mill -i CoupledL2.testtop.zhujiang.compile`
- `make test-top-zhujiang-singlecore`
- `mill -i CoupledL2.testtop.l2.compile`
- `mill -i CoupledL2.test.compile`

生成的 `build/` 和 `out/` 目录被 `.gitignore` 忽略，没有提交。
